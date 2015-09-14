module Main where

import Control.Monad.Identity
import Control.Monad (liftM, unless, when)
import Control.Monad.Writer.Lazy
import System.Directory (doesFileExist)

type UpgradeOp a = IO a
type TableOp a = IO a
type TableName = String
type ColumnName = String

upgrade :: UpgradeOp ()
upgrade = do 
    cond <- hasTable "pepa"
    when cond $
        withTable "pepa" $ do
            createColumn "aColumn"
            alterName "badName" "goodName"
    sql "some SQL"

withTable :: TableName -> TableOp ()-> UpgradeOp ()
withTable tableName _ = putStrLn $ "do something with " ++ tableName 

createColumn :: String -> TableOp ()
createColumn = undefined

alterName :: String -> String -> TableOp ()
alterName = undefined 

hasTable :: ColumnName -> UpgradeOp Bool
hasTable = doesFileExist

sql :: String -> UpgradeOp ()
sql = putStrLn 


main = putStr "hello World"

