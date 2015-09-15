--
-- Simple example using the StateT monad transformer.
--

import Control.Exception
import Control.Monad.State
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf

type DB = StateT Connection IO ()

newtype DBCmd a = DBCmd { getDB :: StateT Connection IO a  }

instance Monad DBCmd where
    m >>= f  = DBCmd $ getDB m >>= getDB . f
    return x = DBCmd $ return x

sql :: String -> DB
sql q = do c <- get; liftIO $ run c q [] >> return ()

query :: ([[SqlValue]] -> a) -> String -> DBCmd a
query f q =
    DBCmd $ do c <- get
               r <- liftIO $ quickQuery c q []
               return $ f r

whenC :: DBCmd Bool -> DBCmd () -> DB
whenC p cmd = do b <- getDB p
                 when b $ getDB cmd

tableExists :: String -> DBCmd Bool
tableExists tab =
    query (\xs -> length xs == 1) $
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab

addColumn :: String -> String -> String -> DBCmd ()
addColumn tab col typ =
    DBCmd $ sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: DB
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $ do
            addColumn "DOC" "UUID" "VARCHAR(75)"

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = void . (execStateT upgrade)
