module Api.State where

import Api.Domain
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Servant

data State = State
    { store :: TVar Store
    }

type AppM = ReaderT State Handler

newEmptyStateStore :: IO (TVar Store)
newEmptyStateStore = atomically $ newTVar emptyStore

newEmptyState :: IO State
newEmptyState = fmap (\var -> State{store = var}) newEmptyStateStore

readStore :: AppM Store
readStore = do
    state <- ask
    result <- (liftIO . atomically . readTVar . store) state
    return result

writeStore :: Store -> AppM ()
writeStore newStore = do
    state <- ask
    _ <- liftIO . atomically $ writeTVar (store state) newStore
    return ()
