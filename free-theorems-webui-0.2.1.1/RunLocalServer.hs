import System.Process (system)
import Data.List

import Paths_free_theorems_webui

main = do
   -- Locate the Python script in the data directory and run it.
   fname <- getDataFileName "runLocalServer.py"
   bin_dir <- getBinDir
   data_dir <- getDataDir
   system $ intercalate " " ["python", fname, bin_dir, data_dir]
