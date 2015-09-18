--
-- Simple example using the StateT monad transformer.
--

import Control.Exception
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf

data Ctx c b m = Ctx { getConn :: c
                     , runDDL :: c -> String -> m ()
                     , runDQL :: c -> String -> m [b] }

type DB c b m = ReaderT (Ctx c b m) m ()

newtype DBCmd c b m a = DBCmd { getDB :: ReaderT (Ctx c b m) m a  }

instance Monad m' => Monad (DBCmd c b m') where
    m >>= f = DBCmd $ getDB m >>= getDB . f
    return  = DBCmd . return

sql :: String -> DB c b IO
sql q = do c <- ask
           liftIO $ (runDDL c) (getConn c) q

query :: ([b] -> a) -> String -> DBCmd c b IO a
query f q =
    DBCmd $ do c <- ask
               r <- liftIO $ (runDQL c) (getConn c) q
               return $ f r

whenC :: DBCmd c b IO Bool -> DBCmd c b IO () -> DB c b IO
whenC p cmd = do b <- getDB p
                 when b $ getDB cmd

tableExists :: String -> DBCmd c b IO Bool
tableExists tab =
    query (\xs -> length xs == 1) $
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab

addColumn :: String -> String -> String -> DBCmd c b IO ()
addColumn tab col typ =
    DBCmd $ sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: DB c b IO
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
