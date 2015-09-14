module Main where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Exception
import Control.Monad

data Ctx = Ctx Connection

type DBResult = Either String ()

success :: Monad m => m DBResult
success = return $ Right ()

failure :: (Monad m, Exception e) => e -> m DBResult
failure e = return $ Left $ "Upgrade aborted: " ++ show e

newtype SQLUpgrade a = SQLUpgrade { runSQLUpgrade :: Ctx -> IO a }

type TableName = String

newtype TableOp a = TableOp { runTableOp :: TableName -> SQLUpgrade a }

instance Monad SQLUpgrade where
    u >>= f = SQLUpgrade $ \ ctx -> do a <- runSQLUpgrade u ctx
                                       runSQLUpgrade (f a) ctx

    return x = SQLUpgrade $ \ _ -> return x

instance Monad TableOp where
    u >>= f = TableOp $ \tn -> do 
        a <- runTableOp u tn
        runTableOp (f a) tn

    return x = TableOp $ \_ -> return x

sql :: String -> SQLUpgrade DBResult
sql q = SQLUpgrade $ \ (Ctx c) -> (run c q [] >> success) `catchSql` failure

hasTable :: String -> SQLUpgrade Bool
hasTable tableName = 
    SQLUpgrade $ \ (Ctx c) -> do
        tables <- getTables c
        return $ tableName `elem` tables

withTable :: TableName -> TableOp a -> SQLUpgrade a
withTable tn ops = SQLUpgrade $ runSQLUpgrade (runTableOp ops tn) 
    
addColumn :: String -> TableOp DBResult
addColumn cn = TableOp $ \ tn ->  
    sql $ "alter table " ++ tn ++ " add column " ++ cn ++ " VARCHAR(75)"

(∧) :: SQLUpgrade DBResult -> SQLUpgrade DBResult -> SQLUpgrade DBResult
u ∧ v = SQLUpgrade $ \ ctx -> do a <- runSQLUpgrade u ctx
                                 case a of
                                   Left msg -> return $ Left msg
                                   Right () -> runSQLUpgrade v ctx

(∨) :: SQLUpgrade DBResult -> SQLUpgrade DBResult -> SQLUpgrade DBResult
u ∨ v = SQLUpgrade $ \ ctx -> do a <- runSQLUpgrade u ctx
                                 case a of
                                   Left _ -> runSQLUpgrade v ctx
                                   Right () -> success

when :: Bool -> SQLUpgrade DBResult -> SQLUpgrade DBResult
when b next = SQLUpgrade $ \ ctx -> 
    if b 
        then runSQLUpgrade next ctx
        else success

upgrade :: SQLUpgrade DBResult
upgrade = do
    sql "CREATE TABLE Doc (id INTEGER NOT NULL, title VARCHAR(75), data BLOB)"
    withTable "Doc" $ do
        addColumn "fileName"
        addColumn "other"

--    b <- hasTable "Doc"
--    when b $ do 
--        sql "ALTER TABLE Doc ADD COLUMN fileName VARCHAR(75)"
--        sql "UPDATE Doc SET fileName = title"    
    sql "CREATE TABLE Otra (id INTEGER NOT NULL, title VARCHAR(75), data BLOB)"

main :: IO ()
main = do
  c <- connectSqlite3 "upgrade.db"
  r <- runSQLUpgrade upgrade (Ctx c)
  print r
  commit c
  disconnect c
