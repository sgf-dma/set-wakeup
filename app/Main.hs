{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Data.Foldable
import Data.Ini
import Options.Applicative
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Map.Strict as S
import qualified Data.Attoparsec.Text as A
import Control.Monad
import Control.Monad.Except


-- | System's ACPI wakeup config.
acpiFile :: FilePath
acpiFile            = "/proc/acpi/wakeup"

-- | Config file.
configFile :: FilePath
configFile          = "./1.cfg"

-- | ACPI wakeup method name.
type MethodT         = T.Text

-- | Wakeup method state (enabled/disabled).
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

-- | Add states preferring left argument and keeping `Loaded` state, if equal.
-- This is intended to be used with 'foldl'.
addState :: MethodState -> MethodState -> MethodState
addState On        LoadedOn     = LoadedOn
addState Off       LoadedOff    = LoadedOff
addState LoadedOn  On           = LoadedOn
addState LoadedOff Off          = LoadedOff
addState x         _            = x

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
-- | Parse method state from config file.
parseMethodState :: A.Parser MethodState
parseMethodState = (pure On       <* parseOn) <|> (pure Off       <* parseOff)
-- | Parse method state loaded from system. Returns 'LoadedOn' and
-- 'LoadedOff'.
loadMethodState :: A.Parser MethodState
loadMethodState  = (pure LoadedOn <* parseOn) <|> (pure LoadedOff <* parseOff)

-- | Map with states for each wakeup method.
type MethodMap      = S.Map MethodT MethodState
-- | Map with list of states (loaded from different sources: system, config
-- file, command-line arguments) for each wakeup method.
type MethodMapL     = S.Map MethodT [MethodState]

-- | Parse ACPI wakeup file (@/proc/acpi/wakeup@ usually) and create a map
-- with states of each method.
parseWakeup :: MethodMapL -> T.Text -> MethodMapL
parseWakeup xm      = foldr go xm . T.lines
  where
    go :: T.Text -> MethodMapL -> MethodMapL
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
generateOpts :: MethodMapL -> Parser MethodMapL
generateOpts        = sequenceA . S.mapWithKey go
  where
    go :: MethodT -> [MethodState] -> Parser [MethodState]
    go k xs         = option (fmap (: xs) auto)
                        (  long (T.unpack (T.toLower k))
                        <> value xs
                        <> metavar (show On ++ "|" ++ show Off)
                        <> help ("Enable or disable "
                            ++ show k ++ " wakeup method."))

-- | Parse ini config file. Any method not available on current system is an
-- error.
parseConfig :: [T.Text]     -- ^ Methods available on current system.
            -> MethodMapL -> Ini -> Either String MethodMapL
parseConfig ts zm0 ini = do
    ks <- keys "Main" ini
    foldrM go zm0 ks
  where
    go :: T.Text -> MethodMapL -> Either String MethodMapL
    go k zm
      | k `elem` ts = do
        x <- parseValue "Main" k parseMethodState ini
        return (S.insertWith (++) k [x] zm)
      | otherwise   = throwError $ "Unknown method "
                            ++ show k ++ " in ini file."

-- | Set methods, which state differs from current system's value.
workT :: MethodMap -> IO ()
workT xs             = do
    print (S.toList xs)
    let ys = S.toList . S.filter isChanged $ xs
    print ys

-- | Read system's ACPI methods and their states, then parse ini config file,
-- then parse command-line arguments. The latter overrides the former.
main_ :: IO ()
main_               = do
            c <- T.readFile acpiFile
            let ac          = parseWakeup S.empty c
                acMethods   = S.keys ac
            ini <- readIniFile configFile
            cf  <- either error return (ini >>= parseConfig acMethods ac)
            let opts = S.map (foldl1 addState) <$> generateOpts cf
            join . execParser $
                info (helper <*> (workT <$> opts))
                (  fullDesc
                <> header "Helper for systemd wakeup service."
                <> progDesc "Enable or disable selected wakeup methods." )
            return ()

main :: IO ()
main = main_

