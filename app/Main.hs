
module Main where

import Lib

import Data.Maybe
import Control.Monad
import Data.List.Split
import Options.Applicative

import Data.ConfigFile

acpiFile :: FilePath
acpiFile            = "/proc/acpi/wakeup"

main :: IO ()
main                = join . execParser $
                        info (helper <*> opts)
                        (  fullDesc
                        <> header "Helper for systemd wakeup service."
                        <> progDesc "Enable or disable selected wakeup methods." )
  where
    opts :: Parser (IO ())
    opts            = work
                        <$> many (strOption
                            (  long "disable"
                            <> short 'd'
                            <> metavar "METHOD"
                            <> help "Disable wakeup method." ))
                        <*> many (strOption
                            (  long "enable"
                            <> short 'e'
                            <> metavar "METHOD"
                            <> help "Enable wakeup method." ))

type Method         = String

work :: [Method] -> [Method] -> IO ()
work ds es = do
    c <- readFile acpiFile
    let as = catMaybes . map (checkMethod ds es) $ (lines c)
    print as

-- Reader?
checkMethod :: [Method] -> [Method] -> String -> Maybe Method
checkMethod ds es l = case wordsBy (`elem` " \t") l of
                        m : _ : "*enabled"  : _ ->
                          if (m `elem` ds) then Just m else Nothing
                        m : _ : "*disabled" : _ ->
                          if (m `elem` es) then Just m else Nothing
                        _ -> Nothing

