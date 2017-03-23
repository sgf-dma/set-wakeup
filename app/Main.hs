
module Main where

import Lib
import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.List.Split
import Options.Applicative
import Data.Char

import Control.Monad.Except
import Data.ConfigFile
import qualified Data.Map.Strict as S

acpiFile :: FilePath
acpiFile            = "/proc/acpi/wakeup"


cf :: FilePath
cf = "./1.cfg"

mainC1 :: IO ()
mainC1  = do
    m <- readfile emptyCP cf
    -- _ <- runExceptT m
    r <- runExceptT $ do
      cf <- m
      return cf
      d <- get cf "DEFAULT" "disable"
      --e <- get cf "DEFAULT" "enable"
      return (d :: String)
      {---e <- get "DEFAULT" "enable" cf
      return (d, e)-}
    print r
    return ()

-- | ACPI wakeup method name.
type Method         = String

-- | Wakeup method state (enabled/disabled).
data MethodState    = On            -- ^ @On@ state to be applied.
                    | Off           -- ^ @Off@ state to be applied.
                    | DefaultOn     -- ^ @On@ state already set in wakeup file.
                    | DefaultOff    -- ^ @Off@ state already set in wakeup file.
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
instance Eq MethodState where
    On          == On           = True
    On          == DefaultOn    = True
    DefaultOn   == On           = True
    DefaultOn   == DefaultOn    = True
    Off         == Off          = True
    Off         == DefaultOff   = True
    DefaultOff  == Off          = True
    DefaultOff  == DefaultOff   = True
    _           == _            = False

-- | Map with states of each wakeup method.
type MethodMap      = S.Map Method MethodState

-- | Parse ACPI wakeup file (@/proc/acpi/wakeup@ usually) and create a map
-- with states of each method.
parseWakeup :: String -> MethodMap
parseWakeup         = foldr go S.empty . lines
  where
    go :: String -> MethodMap -> MethodMap
    go l            = case wordsBy (`elem` " \t") l of
                        m : _ : "*enabled"  : _ -> S.insert (map toLower m) On
                        m : _ : "*disabled" : _ -> S.insert (map toLower m) Off
                        _                       -> id

-- | Generate options based on available methods on current system:
--  * Default values for each option is current method state.
--  * I have used regular options instead of flags, because flag may only
--  /switch/ method from current value, but it does not allow to /specify/
--  desired method state and leave for the program to apply it.
generateOpts :: MethodMap -> Parser MethodMap
generateOpts        = S.foldrWithKey go (pure S.empty)
  where
    go :: Method -> MethodState -> Parser MethodMap -> Parser MethodMap
    go k x zs       = S.insert k <$>
                        option auto
                            ( long k <> value x <> metavar (show On ++ "|" ++ show Off) <>
                              help ("Enable or disable "
                                ++ map toUpper k ++ " wakeup method."))
                            <*>
                        zs

work6 :: MethodMap -> IO ()
work6 xs              = do
    print (S.toList xs)

main6 :: IO ()
main6   = do
            ac <- readFile acpiFile
            let opts = generateOpts (parseWakeup ac)
            join . execParser $
                info (helper <*> (work6 <$> opts))
                (  fullDesc
                <> header "Helper for systemd wakeup service."
                <> progDesc "Enable or disable selected wakeup methods." )

main :: IO ()
main = main6

