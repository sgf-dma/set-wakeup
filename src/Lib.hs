{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lib
  where

import Data.Monoid
import Data.Foldable
import Data.Ini
import Options.Applicative
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Attoparsec.Text   as T
import qualified Data.Map.Strict        as M
import Control.Monad
import Control.Monad.Except
import System.Directory

import Development.Shake.Command
import Sgf.Development.Shake.Command

import Data.Maybe
import Control.Monad.Identity


-- Redefine Lenses.
type Lens a b       = forall f. Functor f => (b -> f b) -> a -> f a

view :: Lens a b -> a -> b
view l              = fromJust . getLast . getConst . l (Const . Last . Just)

modify :: Lens a b -> (b -> b) -> a -> a
modify l f          = runIdentity . l (Identity . f)

set :: Lens a b -> b -> a -> a
set l s            = modify l (const s)


-- | ACPI wakeup method name.
type Method         = T.Text

data MState         = On | Off
  deriving (Show, Eq)

-- | Used for parsing command line options's arguments.
instance Read MState where
    readsPrec d     = readParen (d > 10) $ \k -> do
        (s, t) <- lex k
        case s of
          _
            | s `elem` ["On", "on", "Enabled", "enabled"] ->
                return (On, t)
            | s `elem` ["Off", "off", "Disabled", "disabled"] ->
                return (Off, t)
            | otherwise ->
                []

-- | Parse method state.
parseMState :: T.Parser MState
parseMState         = (pure On <* parseOn) <|> (pure Off <* parseOff)
  where
    parseOn :: T.Parser T.Text
    parseOn         = asum . map T.string
                        $ ["On", "on", "enabled", "Enabled"]
    parseOff :: T.Parser T.Text
    parseOff        = asum . map T.string
                        $ ["Off", "off", "disabled", "Disabled"]

data MethodState    = Loaded {_mstate :: MState}
                    | Config {_mstate :: MState}
                    | Cmd    {_mstate :: MState}
  deriving (Show)
mstate :: Lens MethodState MState
mstate f (Loaded x) = fmap (\x' -> Loaded x') (f x)
mstate f (Config x) = fmap (\x' -> Config x') (f x)
mstate f (Cmd x)    = fmap (\x' -> Cmd    x') (f x)

isLoaded :: MethodState -> Bool
isLoaded (Loaded _) = True
isLoaded _          = False

instance Eq MethodState where
    Loaded _ == Loaded _    = True
    Config _ == Config _    = True
    Cmd    _ == Cmd    _    = True
    _        == _           = False
-- | Values with the same constructor are equal. Prefer 'Loaded' over any
-- other value if state is the same. Otherwise, 'Cmd' > 'Config' > 'Loaded'.
instance Ord MethodState where
    compare (Loaded _)  (Loaded _)  = EQ
    compare (Config _)  (Config _)  = EQ
    compare (Cmd _)     (Cmd _)     = EQ
    compare (Loaded xs) y
      | xs == view mstate y         = GT
      | otherwise                   = LT
    compare x           (Loaded ys)
      | ys == view mstate x         = LT
      | otherwise                   = GT
    compare (Config _)  (Cmd _)     = LT
    compare (Cmd _)     (Config _)  = GT

-- | Map with states for each wakeup method.
type MethodMap      = M.Map Method MethodState
-- | Map with list of states (gathered from different sources: system, config
-- file, command-line arguments) for each wakeup method.
type MethodMapL     = M.Map Method [MethodState]

-- | Parse ACPI wakeup file (@\/proc\/acpi\/wakeup@ usually) and create a map
-- with states for each method.
parseWakeup :: MethodMapL -> T.Text -> MethodMapL
parseWakeup xm      = foldr go xm . T.lines
  where
    go :: T.Text -> MethodMapL -> MethodMapL
    go l            =
        case filter (not . T.null) . T.split (`elem` [' ',  '\t']) $ l of
          m : _ : "*enabled"  : _ -> M.insertWith (++) m [Loaded On]
          m : _ : "*disabled" : _ -> M.insertWith (++) m [Loaded Off]
          _                       -> id

-- | Generate options based on available methods on current system:
--
--      * Default values for each option is current method state.
--      * I have used regular options instead of flags, because flag may only
--      /switch/ method from current value, but it does not allow to /specify/
--      desired method state and leave for the program to apply it.
generateOpts :: MethodMapL -> Parser MethodMapL
generateOpts        = sequenceA . M.mapWithKey go
  where
    go :: Method -> [MethodState] -> Parser [MethodState]
    go k xs         = option (fmap ((: xs) . Cmd) auto)
                        (  long (T.unpack (T.toLower k))
                        <> value xs
                        <> metavar (show On ++ "|" ++ show Off)
                        <> help ("Enable or disable "
                            ++ show k ++ " wakeup method."))

-- | 'Ini' config section, where i expect to find settings.
cfSection :: T.Text
cfSection           = "Main"

-- | Parse ini config file. Any method not available on current system is an
-- error.
parseConfig :: [T.Text]     -- ^ Methods available on current system.
            -> MethodMapL -> Ini -> Either String MethodMapL
parseConfig ts zm0 ini
  | null (sections ini) = return zm0
  | otherwise           = keys cfSection ini >>= foldrM go zm0
  where
    go :: T.Text -> MethodMapL -> Either String MethodMapL
    go k zm
      | k `elem` ts = do
            x <- parseValue cfSection k parseMState ini
            return (M.insertWith (++) k [Config x] zm)
      | otherwise   = throwError $ "Error: unknown method "
                            ++ show k ++ " in ini file."

-- | Print 'MethodMap' in 'Ini'-compatible format.
printConfig :: MethodMap -> T.Text
printConfig         = T.unlines . (cfSection' :) . M.elems . M.mapWithKey go
  where
    cfSection' :: T.Text
    cfSection'      = '[' `T.cons` cfSection `T.append` "]"
    go :: Method -> MethodState -> T.Text
    go x s          = x `T.append` T.pack ('=' : show (view mstate s))

-- | Verify, that given textual config may be parsed to given 'MethodMap'.
verifyConfig :: MethodMap -> T.Text -> Either String T.Text
verifyConfig xm ys  = do
    ini <- parseIni ys
    ym <- parseConfig (M.keys xm) M.empty ini
    --let ym' = M.insert "LID" [Config On] ym
    --let ym' = M.insertWith (++) "EHC1" [Off] ym
    when (M.map (view mstate) xm /= M.map (view mstate) (mergeMethodMap ym)) . throwError . unlines $
      [ "Error: verify of generated config fails."
      , "Generated config: " ++ show ym
      , "Read from system: " ++ show xm
      ]
    return ys

-- | Read ini config file returning empty 'Ini', if there is no config file.
readConfig :: FilePath -> IO (Either String Ini)
readConfig cfFile   = do
    b <- doesFileExist cfFile
    c <- if b then T.readFile cfFile else return T.empty
    return (parseIni c)

-- | Add options related to config file.
configOpts :: MethodMapL -> Parser FilePath
configOpts xm    =
    let mcf = mergeMethodMap xm
        tcf = T.unpack <$> verifyConfig mcf (printConfig mcf)
        optGen  = long "generate-config"
                    <> help "Generate config based on current system's values."
    in  either (\x -> abortOption (ErrorMsg x) optGen)
               (\x -> infoOption  x optGen)
               tcf
        <*> option auto (long "config" <> showDefault <> value "" <> help "Path to config file.")

-- | Group elements matching predicate to the left preserving their order.
groupl :: (a -> Bool) -> [a] -> [a]
groupl p xs          = let (fl, fr) = foldr go (id, id) xs
                      in  fl . fr $ []
  where
    --go :: a -> ([a] -> [a], [a] -> [a]) -> ([a] -> [a], [a] -> [a])
    go x (zl, zr)
      | p x         = ((x :) . zl,         zr)
      | otherwise   = (        zl, (x :) . zr)

mergeMethodMap :: MethodMapL -> MethodMap
mergeMethodMap      = M.map (foldl1 max)

data Conf           = Conf
                        { configFile :: FilePath
                        , methodMapL :: MethodMapL
                        }
  deriving (Show)

workT :: FilePath -> Conf -> IO ()
workT acFile conf     = do
    let xm = methodMapL conf
        cfFile = configFile conf
    print xm
    let acMethods   = M.keys xm
    ini <- readConfig cfFile
    cf  <- either error return (ini >>= parseConfig acMethods xm)
    print cf
    let xs = mergeMethodMap cf
        ys = M.toList . M.filter (not . isLoaded) $ xs
    print ys
    --forM_ ys $ \x -> unit $ rootCmd Shell ("echo" :: String) (T.unpack . fst $ x) acFile
    forM_ ys $ \x -> unit $ privCmd "dmitriym" Shell ["echo", T.unpack (fst x), ">", acFile]
    return ()

main_ :: FilePath -> IO ()
main_ acFile = do
    c <- T.readFile acFile
    let ac   = parseWakeup M.empty c
        opts = Conf <$> configOpts ac <*> generateOpts ac
    join . execParser $
        info (helper <*> (workT acFile <$> opts))
        (  fullDesc
        <> header "Helper for systemd wakeup service."
        <> progDesc "Enable or disable selected wakeup methods." )

