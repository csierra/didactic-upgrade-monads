import Control.Monad.Free

data TableDSL next =
	AddColumn String String next |
	AlterType String String next |
	AlterName String String next
		deriving (Functor)

type TableOp = Free TableDSL

withTable :: String -> TableOp () -> Upgrade a ()
withTable t ops = liftF $ Table t ops ()

addColumn :: String -> String -> TableOp ()
addColumn o n = liftF $ AddColumn o n ()

alterName :: String -> String -> TableOp ()
alterName o n = liftF $ AlterName o n ()

alterType :: String -> String -> TableOp ()
alterType n t = liftF $ AlterType n t ()

data DSL a next = 
	Query String ([[a]] -> next) | 
	DDL String next | 
	Table String (TableOp ()) next |
	Commit
		deriving (Functor)

type Upgrade a = Free (DSL a)

query :: String -> Upgrade a [[a]]
query q = liftF $ Query q id

sql :: String -> Upgrade a ()
sql q = liftF $ DDL q ()

commit :: Upgrade a ()
commit = liftF $ Commit

liftF :: (Functor f) => f r -> Free f r
liftF command = Impure (fmap Pure command)

interpretTable :: String -> TableOp () -> IO ()
interpretTable name (Impure (AddColumn n t next)) = print ("ALTER TABLE " ++ name ++ " ADD COLUMN " ++ n ++ " " ++ t) >> interpretTable name next 
interpretTable name (Impure (AlterType n t next)) = print ("ALTER TABLE " ++ name ++ " ALTER COLUMN " ++ n ++ " " ++ t) >> interpretTable name next
interpretTable name (Impure (AlterName n t next)) = print ("ALTER TABLE " ++ name ++ " ALTER COLUMN NAME " ++ n ++ " " ++ t) >> interpretTable name next
interpretTable name (Pure ()) = print "END TABLE"

interpret :: Upgrade a () -> IO ()
interpret (Impure (Query s f)) = print s >> interpret (f [])
interpret (Impure (DDL s next)) = print s >> interpret next
interpret (Impure (Table name ops next)) = interpretTable name ops >> interpret next
interpret (Pure ()) = print "END PROCESS"

-----------

hasTable :: String -> Upgrade a Bool
hasTable s = do
	rows <- query $ "select * from Tables where " ++ s
	return ((length rows) > 0)

whenC :: Upgrade a Bool -> Upgrade a () -> Upgrade a ()
whenC cond next = do
	b <- cond
	when b $ next

----------

upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (hasTable "DOC") $
    withTable "DOC" $ do 
      addColumn "UUID" "VARCHAR(75)"
      alterType "NAME" "VARCHAR(100)"
  sql "INSERT INTO DOC (ID, NAME, UUID) values (1, 'uno', 'UUID')"	


main :: IO ()
main = undefined
