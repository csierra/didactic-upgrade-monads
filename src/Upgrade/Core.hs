module Upgrade.Core where

import Control.Monad.Reader

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
