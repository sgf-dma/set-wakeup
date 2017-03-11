
import Lib

-- | System's ACPI wakeup config.
acpiFile :: FilePath
acpiFile            = "/proc/acpi/wakeup"

-- | Config file.
configFile :: FilePath
configFile          = "./1.cfg"

main :: IO ()
main = main_ acpiFile configFile

