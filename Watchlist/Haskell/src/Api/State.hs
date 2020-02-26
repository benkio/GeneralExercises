module Api.State where

import Api.Domain
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Servant
import Control.Monad.Reader

data State = State {
  store :: TVar Store
  }

type AppM = ReaderT State Handler

newEmptyStateStore :: IO (TVar Store)
newEmptyStateStore = atomically $ newTVar emptyStore

newEmptyState :: IO State
newEmptyState = fmap (\var -> State { store = var }) newEmptyStateStore
