module Upgrade.Util where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Upgrade.Core

sqlite3Context :: Connection -> Ctx IO SqlValue
sqlite3Context c = Ctx { runDDL  = \q -> void $ run c q []
                       , runDQL  = \q -> quickQuery c q []
                       }

runSqlite3 :: DBCmd () SqlValue -> IO ()
runSqlite3 upgrade =
    bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = runReaderT upgrade . sqlite3Context

stdoutContext :: (String -> [[v]]) -> Ctx IO v
stdoutContext f = Ctx { runDDL = putStrLn
                      , runDQL = \q -> putStrLn q >> return (f q)
                      }

runStdout :: (String -> [[v]]) -> DBCmd () v -> IO ()
runStdout f upgrade = runReaderT upgrade (stdoutContext f)
