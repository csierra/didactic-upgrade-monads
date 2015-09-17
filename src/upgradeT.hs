--
-- Simple example using the StateT monad transformer.
--

import Control.Exception
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf

type DB m = ReaderT Connection m ()

newtype DBCmd m a = DBCmd { getDB :: ReaderT Connection m a  }

instance Monad t => Monad (DBCmd t) where
    m >>= f = DBCmd $ getDB m >>= getDB . f
    return  = DBCmd . return

sql :: String -> DB IO
sql q = do c <- ask
           liftIO $ run c q []
           return ()

query :: ([[SqlValue]] -> a) -> String -> DBCmd IO a
query f q =
    DBCmd $ do c <- ask
               r <- liftIO $ quickQuery c q []
               return $ f r

whenC :: DBCmd IO Bool -> DBCmd IO () -> DB IO
whenC p cmd = do b <- getDB p
                 when b $ getDB cmd

tableExists :: String -> DBCmd IO Bool
tableExists tab =
    query (\xs -> length xs == 1) $
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab

addColumn :: String -> String -> String -> DBCmd IO ()
addColumn tab col typ =
    DBCmd $ sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: DB IO
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $ do
            addColumn "DOC" "UUID" "VARCHAR(75)"

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = void . (runReaderT upgrade)
