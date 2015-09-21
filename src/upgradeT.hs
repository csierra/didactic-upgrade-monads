--{-# LANGUAGE ExistentialQuantification, RankNTypes, MultiParamTypeClasses #-}
--
-- Simple example using the ReaderT monad transformer.
--

import Control.Exception
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap, when)
import Data.Convertible.Base


data Ctx m v = Ctx { runDDL :: String -> m ()
                   , runDQL :: String -> m [[v]]
                   }

type DBCmd a v =
    forall m . (Functor m, Monad m) => ReaderT (Ctx m v) m a

sql :: String -> DBCmd () v
sql q = do c <- ask
           void . lift $ runDDL c q

query :: String -> DBCmd [[v]] v
query q = do c <- ask
             lift $ runDQL c q

whenC :: DBCmd Bool v -> DBCmd () v -> DBCmd () v
whenC p cmd = do x <- p
                 when x cmd

tableExists :: String -> DBCmd Bool v
tableExists tab = fmap isNonEmpty $ query sqlQuery
    where sqlQuery =
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab
          isNonEmpty xs = length xs > 0

addColumn :: String -> String -> String -> DBCmd () v
addColumn tab col typ =
    sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: DBCmd () v
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $
        addColumn "DOC" "UUID" "VARCHAR(75)"
  sql "INSERT INTO DOC (ID, NAME, UUID) values (1, 'uno', 'UUID')"

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = runReaderT upgrade . createCtx
          createCtx c = Ctx { runDDL  = \q -> void $ run c q []
                            , runDQL  = \q -> quickQuery c q []
                            }
