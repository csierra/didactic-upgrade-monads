--
-- Simple example using the StateT monad transformer.
--

import Control.Exception
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf

data Ctx c b = Ctx { getConn :: c
                   , runDDL :: c -> String -> IO ()
                   , runDQL :: c -> String -> IO b }

type DB c b m = ReaderT (Ctx c b) m ()

newtype DBCmd c b m a = DBCmd { getDB :: ReaderT (Ctx c b) m a  }

instance Monad m' => Monad (DBCmd c b m') where
    m >>= f = DBCmd $ getDB m >>= getDB . f
    return  = DBCmd . return

sql :: String -> DB Connection [[SqlValue]] IO
sql q = do c <- ask
           liftIO $ (runDDL c) (getConn c) q

query :: ([[SqlValue]] -> a) -> String -> DBCmd Connection [[SqlValue]] IO a
query f q =
    DBCmd $ do c <- ask
               r <- liftIO $ (runDQL c) (getConn c) q
               return $ f r

whenC :: DBCmd Connection [[SqlValue]] IO Bool -> DBCmd Connection [[SqlValue]] IO () -> DB Connection [[SqlValue]] IO
whenC p cmd = do b <- getDB p
                 when b $ getDB cmd

tableExists :: String -> DBCmd Connection [[SqlValue]] IO Bool
tableExists tab =
    query (\xs -> length xs == 1) $
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab

addColumn :: String -> String -> String -> DBCmd Connection [[SqlValue]] IO ()
addColumn tab col typ =
    DBCmd $ sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: DB Connection [[SqlValue]] IO
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $ do
            addColumn "DOC" "UUID" "VARCHAR(75)"

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = void . (runReaderT upgrade) . createCtx
          createCtx c = Ctx { getConn = c
                            , runDDL  = \c q -> run c q [] >> return ()
                            , runDQL  = \c q -> quickQuery c q [] }
