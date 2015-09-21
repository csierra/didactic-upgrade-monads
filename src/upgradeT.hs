--{-# LANGUAGE ExistentialQuantification, RankNTypes, MultiParamTypeClasses #-}
--
-- Simple example using the StateT monad transformer.
--

import Control.Exception
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap, when)
import Data.Convertible.Base


data Ctx m = Ctx { runDDL :: String -> m ()
                 , runDQL :: String -> m [[SqlValue]]
                 }

type DBCmd a = forall m. (Monad m) => ReaderT (Ctx m) m a

type Value = SqlValue
fromValue = fromSql


sql :: String -> DBCmd ()
sql q = do
    c <- ask
    lift $ runDDL c q

query :: String -> DBCmd [[Value]]
query q = do
    c <- ask
    lift $ runDQL c q

whenC :: DBCmd Bool -> DBCmd () -> DBCmd ()
whenC p cmd = p >>= \x -> when x cmd


tableExists :: String -> DBCmd Bool
tableExists tab = do
    tables <- query $ printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab
    return $ length tables == 1

addColumn :: String -> String -> String -> DBCmd ()
addColumn tab col typ =
    sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: DBCmd ()
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $
            addColumn "DOC" "UUID" "VARCHAR(75)"
  sql "INSERT INTO DOC (ID, NAME, UUID) values (1, 'uno', 'UUID')"

list :: DBCmd [Value]
list = query "Select * from Doc" >>= \(x:xs) -> return x

--test = connectSqlite3 "upgrade.db" >>= \c -> quickQuery c "select * from Doc" []
test = bracket (connectSqlite3 "upgrade.db") close runUpgrade >>= \(x:xs) -> print ((fromValue x::Int) + 1)
    where close c = commit c >> disconnect c
          runUpgrade = runReaderT list . createCtx
          createCtx c = Ctx { runDDL  = \q -> void $ run c q []
                            , runDQL  = \q -> quickQuery c q []
                            }

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = runReaderT upgrade . createCtx
          createCtx c = Ctx { runDDL  = \q -> void $ run c q []
                            , runDQL  = \q -> quickQuery c q []
                            }
