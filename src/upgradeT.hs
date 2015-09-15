--
-- Simple example using the StateT monad transformer.
--

import Control.Exception
import Control.Monad.State
import Database.HDBC
import Database.HDBC.Sqlite3

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
              "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '" ++ tab ++ "'"

addColumn :: String -> String -> String -> DBCmd ()
addColumn tab col typ =
    DBCmd $ sql $ "ALTER TABLE " ++ tab ++ " ADD COLUMN " ++ col ++ " " ++ typ

upgrade :: DB
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $ do
            addColumn "DOC" "UUID" "VARCHAR(75)"

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") disconnect runUpgrade
    where runUpgrade c = do execStateT upgrade c; disconnect c
