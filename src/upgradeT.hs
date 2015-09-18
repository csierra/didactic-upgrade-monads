--{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
--
-- Simple example using the ReaderT monad transformer.
--

import Control.Applicative (Applicative(..))
import Control.Exception
import Control.Monad       (liftM, ap)
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf

data Ctx m v = Ctx { runDDL :: String -> m ()
                   , runDQL :: String -> m [v]
                   }

type DB m v = ReaderT (Ctx m v) m ()

newtype DBCmd m v a = DBCmd { getDB :: ReaderT (Ctx m v) m a  }

instance Monad m' => Monad (DBCmd m' v) where
    m >>= f = DBCmd $ getDB m >>= getDB . f
    return  = DBCmd . return

instance (Monad m) => Functor (DBCmd m v) where
    fmap = liftM

instance (Monad m) => Applicative (DBCmd m v) where
    pure  = return
    (<*>) = ap

type Upgrade = forall m v . Monad m => DB m v
type UpgradeCommand a = forall m v . Monad m => DBCmd m v a

sql :: String -> Upgrade
sql q = do c <- ask
           lift $ runDDL c q

query :: Monad m => ([v] -> a) -> String -> DBCmd m v a
query f q =
    DBCmd $ do c <- ask
               r <- lift $ runDQL c q
               return $ f r

whenC :: UpgradeCommand Bool -> UpgradeCommand () -> Upgrade
whenC p cmd = do b <- getDB p
                 when b $ getDB cmd

tableExists :: String -> UpgradeCommand Bool
tableExists tab =
    query (\xs -> length xs == 1) $
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab

addColumn :: String -> String -> String -> UpgradeCommand ()
addColumn tab col typ =
    DBCmd $ sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ

upgrade :: Upgrade
upgrade = do
  sql "CREATE TABLE DOC (ID PRIMARY KEY, NAME VARCHAR(75) NOT NULL)"
  whenC (tableExists "DOC") $
            addColumn "DOC" "UUID" "VARCHAR(75)"

main :: IO ()
main = bracket (connectSqlite3 "upgrade.db") close runUpgrade
    where close c = commit c >> disconnect c
          runUpgrade = void . runReaderT upgrade . createCtx
          createCtx c = Ctx { runDDL  = \q -> void $ run c q []
                            , runDQL  = \q -> quickQuery c q []
                            }
