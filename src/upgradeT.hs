--
-- Simple example using the StateT monad transformer.
--

import Control.Exception
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf

data Ctx c  = Ctx { getConn :: c }

type DB c m = ReaderT (Ctx c) m ()

newtype DBCmd c m a = DBCmd { getDB :: ReaderT (Ctx c) m a  }

instance Monad m' => Monad (DBCmd c m') where
    m >>= f = DBCmd $ getDB m >>= getDB . f
    return  = DBCmd . return

sql :: String -> DB Connection IO
sql q = do c <- ask
           liftIO $ run (getConn c) q []
           return ()

query :: ([[SqlValue]] -> a) -> String -> DBCmd Connection IO a
query f q =
    DBCmd $ do c <- ask
               r <- liftIO $ quickQuery (getConn c) q []
               return $ f r

whenC :: DBCmd Connection IO Bool -> DBCmd Connection IO () -> DB Connection IO
whenC p cmd = do b <- getDB p
                 when b $ getDB cmd

tableExists :: String -> DBCmd Connection IO Bool
tableExists tab =
    query (\xs -> length xs == 1) $
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab

addColumn :: String -> String -> String -> DBCmd Connection IO ()
addColumn tab col typ =
    DBCmd $ sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: DB Connection IO
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $ do
            addColumn "DOC" "UUID" "VARCHAR(75)"

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = void . (runReaderT upgrade) . Ctx
