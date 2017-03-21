{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

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


-- | ACPI wakeup method name.
type Method         = T.Text

-- | Wakeup method state.
data MethodState    = On        -- ^ @On@ state to be applied.
                    | Off       -- ^ @Off@ state to be applied.
                    | LoadedOn  -- ^ @On@ state read from system.
                    | LoadedOff -- ^ @Off@ state read from system.
  deriving (Show)

instance Eq MethodState where
    On          == On           = True
    On          == LoadedOn     = True
    LoadedOn    == On           = True
    LoadedOn    == LoadedOn     = True
    Off         == Off          = True
    Off         == LoadedOff    = True
    LoadedOff   == Off          = True
    LoadedOff   == LoadedOff    = True
    _           == _            = False
-- | Wheter method state differs from what was loaded from the system.
isChanged :: MethodState -> Bool
isChanged On        = True
isChanged Off       = True
isChanged LoadedOn  = False
isChanged LoadedOff = False

-- | Add states preferring left argument and keeping @Loaded@ state, if equal.
-- This is intended to be used with 'foldl'.
addState :: MethodState -> MethodState -> MethodState
addState On        LoadedOn     = LoadedOn
addState Off       LoadedOff    = LoadedOff
addState LoadedOn  On           = LoadedOn
addState LoadedOff Off          = LoadedOff
addState x         _            = x
-- | Merge different states in 'MethodMapL' and make a 'MethodMap' value.
mergeMethodMap :: MethodMapL -> MethodMap
mergeMethodMap      = M.map (foldl1 addState)

instance Read MethodState where
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
-- | Parse @On@ states read from config.
parseOn :: T.Parser T.Text
parseOn         = asum . map T.string
                    $ ["On", "on", "enabled", "Enabled"]
-- | Parse @Off@ states read from config.
parseOff :: T.Parser T.Text
parseOff        = asum . map T.string
                    $ ["Off", "off", "disabled", "Disabled"]
-- | Parse method state read from config file. Returns only 'On' and 'Off'.
parseMethodState :: T.Parser MethodState
parseMethodState = (pure On       <* parseOn) <|> (pure Off       <* parseOff)
-- | Parse method state loaded from system. Returns 'LoadedOn' and
-- 'LoadedOff'.
loadMethodState :: T.Parser MethodState
loadMethodState  = (pure LoadedOn <* parseOn) <|> (pure LoadedOff <* parseOff)
-- | Print 'Method' and 'MethodState' values in 'Ini'-compatible format.
printMethodState :: Method -> MethodState -> T.Text
printMethodState x s
  | s == On         = x `T.append` T.pack ('=' : show On)
  | s == Off        = x `T.append` T.pack ('=' : show Off)
  | otherwise       = error ("Error: unknown method state: " ++ show s)

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
          m : _ : "*enabled"  : _ -> M.insertWith (++) m [LoadedOn]
          m : _ : "*disabled" : _ -> M.insertWith (++) m [LoadedOff]
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
    go k xs         = option (fmap (: xs) auto)
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
            x <- parseValue cfSection k parseMethodState ini
            return (M.insertWith (++) k [x] zm)
      | otherwise   = throwError $ "Error: unknown method "
                            ++ show k ++ " in ini file."
-- | Print 'MethodMap' in 'Ini'-compatible format.
printConfig :: MethodMap -> T.Text
printConfig         = T.unlines . (cfSection' :) . M.elems
                        . M.mapWithKey printMethodState
  where
    cfSection' :: T.Text
    cfSection'      = '[' `T.cons` cfSection `T.append` "]"

-- | Verify, that given textual config may be parsed to given 'MethodMap'.
verifyConfig :: MethodMap -> T.Text -> Either String T.Text
verifyConfig xm ys  = do
    ini <- parseIni ys
    ym <- parseConfig (M.keys xm) M.empty ini
    let ym' = M.insert "LID" [On] ym
    --let ym' = M.insertWith (++) "EHC1" [Off] ym
    when (xm /= mergeMethodMap ym') . throwError . unlines $
      [ "Error: verify of generated config fails."
      , "Generated config: " ++ show ym'
      , "Read from system: " ++ show xm
      ]
    return ys

-- | Set methods, which state differs from current system's value.
workT :: FilePath -> MethodMap -> IO ()
workT acFile xs     = do
    print (M.toList xs)
    let ys = M.toList . M.filter isChanged $ xs
    print ys
    --forM_ ys $ \x -> unit $ rootCmd Shell ("echo" :: String) (T.unpack . fst $ x) acFile
    forM_ ys $ \x -> unit $ privCmd "dmitriym" Shell ["echo", T.unpack (fst x), ">", acFile]
    return ()

-- | Read ini config file returning empty 'Ini', if there is no config file.
readConfig :: FilePath -> IO (Either String Ini)
readConfig cfFile   = do
    b <- doesFileExist cfFile
    c <- if b then T.readFile cfFile else return T.empty
    return (parseIni c)

-- | Add options related to config file.
addConfigOpts :: MethodMapL -> Parser (a -> a)
addConfigOpts xm    =
    let mcf = mergeMethodMap xm
        tcf = T.unpack <$> verifyConfig mcf (printConfig mcf)
        optGen  = long "generate-config"
                    <> help ("Generate config based on current "
                          ++ "system's values *and* existing config.")
        {-optFile = long "config"
                    <> metavar "PATH"
                    <> help "Path to config file."-}
    in  either (\x -> abortOption (ErrorMsg x) optGen)
               (\x -> infoOption  x optGen)
               tcf

-- | Read system's ACPI methods and their states, parse ini config file, parse
-- command-line arguments (the latter overrides the former) and then apply
-- resulting method states.
-- FIXME: If i want to accept config file as option, i need to change states
-- gathering order: system, cmd, config. Will should i sum them.. does conf +
-- system + cmd work?
main_ :: FilePath -> FilePath -> IO ()
main_ acFile cfFile = do
    c <- T.readFile acFile
    let ac          = parseWakeup M.empty c
        acMethods   = M.keys ac
    ini <- readConfig cfFile
    cf  <- either error return (ini >>= parseConfig acMethods ac)
    let opts = mergeMethodMap <$> (addConfigOpts cf <*> generateOpts cf)
    join . execParser $
        info (helper <*> (workT acFile <$> opts))
        (  fullDesc
        <> header "Helper for systemd wakeup service."
        <> progDesc "Enable or disable selected wakeup methods." )

