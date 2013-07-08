module LJI.Name (
  programName,
  programTitle,
  version
) where

--import qualified Paths_LJI as Path (version)
import Data.Version (showVersion)

version :: String
--version = showVersion Path.version
version = "2.0"

programName :: String
programName = "LJI"

programTitle :: String
programTitle = "Linear Jazz Improvisation Method"
