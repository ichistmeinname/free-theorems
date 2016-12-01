module LogRequests where

import Data.Time.LocalTime
import System.IO
import System.FilePath ((</>))
import Network.CGI
import Text.CSV
import Data.Maybe (fromMaybe, isJust)

import Paths (getLogDirectory)


logRequest inputs = do
   timestamp <- getTimestamp
   maybeHost <- remoteHost
   ip        <- remoteAddr
   log_dir   <- liftIO getLogDirectory
   liftIO $ appendFile (log_dir </> "requests.csv") $ printCSV
     [[ timestamp
      , maybeHost `orIfNothing` ip
      , inputFor "type"
      , inputFor "model"
      , inputFor "style"
      , inputFor "format"
      , show (hasInputFor "hideTypeInstantiations")
      --, inputFor "xtraSrc" -- may contain multiple lines
     ]]

   where inputFor key = fromMaybe "" (lookup key inputs)
         hasInputFor key = isJust (lookup key inputs)


getTimestamp :: CGI String
getTimestamp = do
   t <- liftIO getZonedTime
   return $ show $ zonedTimeToLocalTime t

orIfNothing :: Maybe a -> a -> a
orIfNothing (Just x) y = x
orIfNothing Nothing  y = y
