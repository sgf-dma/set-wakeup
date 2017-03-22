
import Lib

-- | System's ACPI wakeup config.
acpiFile :: FilePath
acpiFile            = "/proc/acpi/wakeup"

main :: IO ()
main = main_ acpiFile

