{-# LANGUAGE NoMonomorphismRestriction #-}
module Paths
   ( getTemporaryDirectory
   , getPathToAdditionalTeXFiles
   , getLogDirectory
   , relativeURLOf
   )
where

{-
 -  root/
 -    |-- cgi-bin/
 -    |-- free-theorems/
 -         |-- lambdaTeX.tex
 -         |-- style.css
 -    |-- tmp/
 -         |-- free-theorems/
 -              |-- requests.csv
 -
 -}

import qualified System.Directory
import Control.Exception.Base (catch, SomeException)

getTemporaryDirectory =
   catch (System.Directory.getTemporaryDirectory)
         (\(_ :: SomeException) -> return ".")

getPathToAdditionalTeXFiles = return "../free-theorems"

getLogDirectory = return "../tmp/free-theorems"

relativeURLOf filename = "../free-theorems/" ++ filename
