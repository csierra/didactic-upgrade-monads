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
 

data Ctx c m = Ctx { getConn :: c
                     , runDDL :: c -> String -> m ()
                     , runDQL :: c -> String -> m [[SqlValue]]
                     }

type DBCmd a = forall c m. (Monad m) => ReaderT (Ctx c m) m a

data Value a = V a

instance Convertible SqlValue (Value a) where 
    safeConvert s = Right $ convert s

sql :: String -> DBCmd ()
sql q = do 
    c <- ask
    lift $ runDDL c (getConn c) q

query :: String -> DBCmd [[Value a]]
query q = do
    c <- ask
    lift $ runDQL c (getConn c) q >>= \x -> return (map (map fromSql) x)

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

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = void . runReaderT upgrade . createCtx
          createCtx c = Ctx { getConn = c
                            , runDDL  = \c q -> void $ run c q []
                            , runDQL  = \c q -> quickQuery c q []
                            }
