
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
data MethodState    = On        -- ^ @On@ state to be applied.
                    | Off       -- ^ @Off@ state to be applied.
                    | LoadedOn  -- ^ @On@ state already set in wakeup file.
                    | LoadedOff -- ^ @Off@ state already set in wakeup file.
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
    On          == LoadedOn     = True
    LoadedOn    == On           = True
    LoadedOn    == LoadedOn     = True
    Off         == Off          = True
    Off         == LoadedOff    = True
    LoadedOff   == Off          = True
    LoadedOff   == LoadedOff    = True
    _           == _            = False
isChanged :: MethodState -> Bool
isChanged On        = True
isChanged Off       = True
isChanged LoadedOn  = False
isChanged LoadedOff = False

-- | Map with states of each wakeup method.
type MethodMap      = S.Map Method MethodState

-- | Parse ACPI wakeup file (@/proc/acpi/wakeup@ usually) and create a map
-- with states of each method.
parseWakeup :: String -> MethodMap
parseWakeup         = foldr go S.empty . lines
  where
    go :: String -> MethodMap -> MethodMap
    go l            = case wordsBy (`elem` " \t") l of
        m : _ : "*enabled"  : _ -> S.insert m LoadedOn
        m : _ : "*disabled" : _ -> S.insert m LoadedOff
        _                       -> id

-- | Generate options based on available methods on current system:
--  * Default values for each option is current method state.
--  * I have used regular options instead of flags, because flag may only
--  /switch/ method from current value, but it does not allow to /specify/
--  desired method state and leave for the program to apply it.
generateOpts :: MethodMap -> Parser MethodMap
generateOpts        = S.foldrWithKey go (pure S.empty)
  where
    ifDiffer :: Eq a => a -> a -> a
    ifDiffer x y
      | x == y      = x
      | otherwise   = y
    go :: Method -> MethodState -> Parser MethodMap -> Parser MethodMap
    go k x zs       = S.insert k <$> option (auto >>= return . ifDiffer x)
                            (  long (map toLower k)
                            <> value x
                            <> metavar (show On ++ "|" ++ show Off)
                            <> help ("Enable or disable "
                                ++ k ++ " wakeup method."))
                        <*> zs

work :: MethodMap -> IO ()
work xs             = do
    print (S.toList xs)
    let ys = S.toList . S.filter isChanged $ xs
    print ys
    --mapM_ (writeFile acpiFile . fst) ys

main_ :: IO ()
main_               = do
            ac <- readFile acpiFile
            let opts = generateOpts (parseWakeup ac)
            join . execParser $
                info (helper <*> (work <$> opts))
                (  fullDesc
                <> header "Helper for systemd wakeup service."
                <> progDesc "Enable or disable selected wakeup methods." )

main :: IO ()
main = main_

