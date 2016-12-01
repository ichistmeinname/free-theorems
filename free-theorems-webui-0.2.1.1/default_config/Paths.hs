module Paths
   ( getTemporaryDirectory
   , getPathToAdditionalTeXFiles
   , getLogDirectory
   , relativeURLOf
   )
where

import Paths_free_theorems_webui (getDataDir)
import qualified System.Directory
import Control.Exception.Base (catch, SomeException)

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = return "C:\\Users\\XFigh_000\\AppData\\Local\\Temp"
--   catch (System.Directory.getTemporaryDirectory)
--         (\(_ :: SomeException) -> return ".")

getPathToAdditionalTeXFiles = getDataDir

getLogDirectory = getTemporaryDirectory

relativeURLOf filename = "../" ++ filename
