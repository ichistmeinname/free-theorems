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

getTemporaryDirectory =
   catch (System.Directory.getTemporaryDirectory)
         (\(_ :: SomeException) -> return ".")

getPathToAdditionalTeXFiles = getDataDir

getLogDirectory = getTemporaryDirectory

relativeURLOf filename = "../" ++ filename
