{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.List.Split
import Options.Applicative
import Data.Char
import Data.Ini
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.Except
import qualified Data.Map.Strict as S
import qualified Data.Attoparsec.Text as A
import Data.Foldable
import Control.Monad.Except

acpiFile :: FilePath
acpiFile            = "/proc/acpi/wakeup"


cf :: FilePath
cf = "./1.cfg"

-- | ACPI wakeup method name.
type MethodT         = T.Text

-- | Wakeup method state (enabled/disabled).
data MethodState    = On        -- ^ @On@ state to be applied.
                    | Off       -- ^ @Off@ state to be applied.
                    | LoadedOn  -- ^ @On@ state read from system.
                    | LoadedOff -- ^ @Off@ state read from system.
  deriving (Show)
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

parseOn :: A.Parser T.Text
parseOn         = asum . map A.string
                    $ ["On", "on", "enabled", "Enabled"]
parseOff :: A.Parser T.Text
parseOff        = asum . map A.string
                    $ ["Off", "off", "disabled", "Disabled"]
parseMethodState :: A.Parser MethodState
parseMethodState = (pure On <* parseOn) <|> (pure Off <* parseOff)
loadMethodState :: A.Parser MethodState
loadMethodState  = (pure LoadedOn <* parseOn) <|> (pure LoadedOff <* parseOff)

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

-- | Map with states of each wakeup method.
type MethodMapT      = S.Map MethodT MethodState
type MethodMapTM     = S.Map MethodT (Maybe MethodState)
type MethodMapTL     = S.Map MethodT [MethodState]

-- | Parse ACPI wakeup file (@/proc/acpi/wakeup@ usually) and create a map
-- with states of each method.
parseWakeupT :: T.Text -> MethodMapT
parseWakeupT        = foldr go S.empty . T.lines
  where
    go :: T.Text -> MethodMapT -> MethodMapT
    go l            =
        case filter (not . T.null) . T.split (`elem` [' ',  '\t']) $ l of
          m : _ : "*enabled"  : _ -> S.insert m LoadedOn
          m : _ : "*disabled" : _ -> S.insert m LoadedOff
          _                       -> id
parseWakeupTL :: T.Text -> MethodMapTL
parseWakeupTL       = foldr go S.empty . T.lines
  where
    go :: T.Text -> MethodMapTL -> MethodMapTL
    go l            =
        case filter (not . T.null) . T.split (`elem` [' ',  '\t']) $ l of
          m : _ : "*enabled"  : _ -> S.insert m [LoadedOn]
          m : _ : "*disabled" : _ -> S.insert m [LoadedOff]
          _                       -> id
parseWakeupTL1 :: MethodMapTL -> T.Text -> MethodMapTL
parseWakeupTL1 xm   = foldr go xm . T.lines
  where
    go :: T.Text -> MethodMapTL -> MethodMapTL
    go l            =
        case filter (not . T.null) . T.split (`elem` [' ',  '\t']) $ l of
          m : _ : "*enabled"  : _ -> S.insertWith (++) m [LoadedOn]
          m : _ : "*disabled" : _ -> S.insertWith (++) m [LoadedOff]
          _                       -> id


-- | Generate options based on available methods on current system:
--
--      * Default values for each option is current method state.
--      * I have used regular options instead of flags, because flag may only
--      /switch/ method from current value, but it does not allow to /specify/
--      desired method state and leave for the program to apply it.
generateOptsT :: MethodMapT -> Parser MethodMapT
generateOptsT        = S.foldrWithKey go (pure S.empty)
  where
    ifDiffer :: Eq a => a -> a -> a
    ifDiffer x y
      | x == y      = x
      | otherwise   = y
    go :: MethodT -> MethodState -> Parser MethodMapT -> Parser MethodMapT
    go k x zs       = S.insert k <$> option (fmap (ifDiffer x) auto)
                            (  long (T.unpack (T.toLower k))
                            <> value x
                            <> metavar (show On ++ "|" ++ show Off)
                            <> help ("Enable or disable "
                                ++ (show k) ++ " wakeup method."))
                        <*> zs
generateOptsTM :: MethodMapTM -> Parser MethodMapTM
generateOptsTM        = S.foldrWithKey go (pure S.empty)
  where
    go :: MethodT -> Maybe MethodState -> Parser MethodMapTM -> Parser MethodMapTM
    go k x zs       = S.insert k <$> option (Just <$> auto)
                            (  long (T.unpack (T.toLower k))
                            <> value x
                            <> metavar (show On ++ "|" ++ show Off)
                            <> help ("Enable or disable "
                                ++ (show k) ++ " wakeup method."))
                        <*> zs

generateOptsTL :: MethodMapTL -> Parser MethodMapTL
generateOptsTL        = S.foldrWithKey go (pure S.empty)
  where
    go :: MethodT -> [MethodState] -> Parser MethodMapTL -> Parser MethodMapTL
    go k xs zm      = S.insert k <$> option (fmap (: xs) auto)
                            (  long (T.unpack (T.toLower k))
                            <> value xs
                            <> metavar (show On ++ "|" ++ show Off)
                            <> help ("Enable or disable "
                                ++ (show k) ++ " wakeup method."))
                        <*> zm
generateOptsTL1 :: MethodMapTL -> Parser MethodMapTL
generateOptsTL1     = sequenceA . S.mapWithKey go2
  where
    go2 :: MethodT -> [MethodState] -> Parser [MethodState]
    go2 k xs        = option (fmap (: xs) auto)
                            (  long (T.unpack (T.toLower k))
                            <> value xs
                            <> metavar (show On ++ "|" ++ show Off)
                            <> help ("Enable or disable "
                                ++ (show k) ++ " wakeup method."))
    go :: MethodT -> [MethodState] -> Parser MethodMapTL -> Parser MethodMapTL
    go k xs zm      = S.insert k <$> option (fmap (: xs) auto)
                            (  long (T.unpack (T.toLower k))
                            <> value xs
                            <> metavar (show On ++ "|" ++ show Off)
                            <> help ("Enable or disable "
                                ++ (show k) ++ " wakeup method."))
                        <*> zm


parseConfig :: MethodMapT -> Ini -> Either String MethodMapT
parseConfig xs ini  = do
    ks <- keys "Main" ini
    sequence . S.mapWithKey (go ini ks) $ xs
  where
    go :: Ini -> [T.Text] -> MethodT -> MethodState
          -> Either String MethodState
    go ini ks k x
      -- `parseValue` parses using `parseOnly (f <* endOfInput)`.
      | k `elem` ks = parseValue "Main" k parseMethodState ini
      | otherwise   = return x
parseConfigTM :: MethodMapTM -> Ini -> Either String MethodMapTM
parseConfigTM xs ini  = do
    ks <- keys "Main" ini
    sequence . S.mapWithKey (go ini ks) $ xs
  where
    go :: Ini -> [T.Text] -> MethodT -> Maybe MethodState
          -> Either String (Maybe MethodState)
    go ini ks k x
      -- `parseValue` parses using `parseOnly (f <* endOfInput)`.
      | k `elem` ks = Just <$> parseValue "Main" k parseMethodState ini
      | otherwise   = return x
parseConfigTL :: MethodMapTL -> Ini -> Either String MethodMapTL
parseConfigTL xm ini  = do
    ks <- keys "Main" ini
    --sequence . S.mapWithKey (\k xs -> (++ xs) <$> go ini ks k) $ xm
    sequence . S.mapWithKey (go ini ks) $ xm
  where
    go :: Ini -> [T.Text] -> MethodT -> [MethodState] -> Either String [MethodState]
    go ini ks k xs
      -- `parseValue` parses using `parseOnly (f <* endOfInput)`.
      | k `elem` ks = fmap (: xs) (parseValue "Main" k parseMethodState ini)
      | otherwise   = return xs
parseConfigTL1 :: [T.Text] -> MethodMapTL -> Ini -> Either String MethodMapTL
parseConfigTL1 ts xm ini  = do
    ks <- keys "Main" ini
    --sequence . S.mapWithKey (go ini ks) $ xm
    foldrM go2 xm ks
  where
    go2 :: T.Text -> MethodMapTL -> Either String MethodMapTL
    go2 k zm
      | k `elem` ts = do
        x <- parseValue "Main" k parseMethodState ini
        return (S.insertWith (++) k [x] zm)
      | otherwise   = throwError ("Unknown key " ++ show k ++ " in ini file.")

workT :: MethodMapT -> IO ()
workT xs             = do
    print (S.toList xs)
    let ys = S.toList . S.filter isChanged $ xs
    print ys

workTL :: MethodMapTL -> IO ()
workTL xs             = do
    print (S.toList xs)
    --let ys = S.toList . S.filter isChanged $ xs
    --print ys

main_ :: IO ()
main_               = do
            ac <- T.readFile acpiFile
            ini <- readIniFile "1.cfg"
            cf <- either error return $
                    ini >>= parseConfig (parseWakeupT ac)
            --let opts = generateOpts (parseWakeup ac)
            let opts = generateOptsT cf
            join . execParser $
                info (helper <*> (workT <$> opts))
                (  fullDesc
                <> header "Helper for systemd wakeup service."
                <> progDesc "Enable or disable selected wakeup methods." )

main_l :: IO ()
main_l               = do
            ac <- T.readFile acpiFile
            let acS = parseWakeupT ac
                ac0 = S.map (const Nothing) acS
            ini <- readIniFile "1.cfg"
            cf <- either error return $
                    ini >>= parseConfigTM ac0
            print cf
            let opts = generateOptsTM cf
                opts1  = mergeMaps1 (S.map (\x -> maybe x (\y -> if x == y then x else y)) acS) <$> opts
            join . execParser $
                info (helper <*> (workT <$> opts1))
                (  fullDesc
                <> header "Helper for systemd wakeup service."
                <> progDesc "Enable or disable selected wakeup methods." )
            return ()

main_r :: IO ()
main_r               = do
            ac <- T.readFile acpiFile
            let acS = parseWakeupTL ac
            ini <- readIniFile "1.cfg"
            cf <- either error return $
                    ini >>= parseConfigTL acS
            print acS
            print cf
            let opts = S.map (foldl1 sumStates) <$> generateOptsTL cf
            join . execParser $
                info (helper <*> (workT <$> opts))
                (  fullDesc
                <> header "Helper for systemd wakeup service."
                <> progDesc "Enable or disable selected wakeup methods." )
            return ()

main_r1 :: IO ()
main_r1               = do
            ac <- T.readFile acpiFile
            let acS = parseWakeupTL1 S.empty ac
            ini <- readIniFile "1.cfg"
            cf <- either error return $
                    ini >>= parseConfigTL1 (S.keys acS) acS
            print acS
            print cf
            let opts = S.map (foldl1 sumStates) <$> generateOptsTL1 cf
            join . execParser $
                info (helper <*> (workT <$> opts))
                (  fullDesc
                <> header "Helper for systemd wakeup service."
                <> progDesc "Enable or disable selected wakeup methods." )
            return ()

-- | Prefer left state and keep `Loaded` state, if equal.
sumStates :: MethodState -> MethodState -> MethodState
sumStates On        LoadedOn   = LoadedOn
sumStates Off       LoadedOff  = LoadedOff
sumStates LoadedOn  On         = LoadedOn
sumStates LoadedOff Off        = LoadedOff
sumStates x         _          = x

mergeMaps :: Ord k => S.Map k (a -> a) -> S.Map k a -> S.Map k a
mergeMaps mf mx = S.foldrWithKey (\k f mz -> S.adjust f k  mz) mx mf
mergeMaps1 :: Ord k => S.Map k (Maybe a -> b) -> S.Map k (Maybe a) -> S.Map k b
mergeMaps1 mf mx = S.foldrWithKey (\k f mz -> S.insert k (f (join (S.lookup k mx))) mz) S.empty mf

main :: IO ()
main = main_r1

