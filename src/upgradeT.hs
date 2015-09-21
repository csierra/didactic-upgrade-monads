import Upgrade.Core
import Upgrade.DSL
import Upgrade.Util

upgrade :: DBCmd () v
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $
        addColumn "DOC" "UUID" "VARCHAR(75)"
  sql "INSERT INTO DOC (ID, NAME, UUID) values (1, 'uno', 'UUID')"

main :: IO ()
main = do runSqlite3 upgrade
          runStdout (const [[1]]) upgrade
