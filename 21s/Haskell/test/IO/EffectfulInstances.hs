{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module IO.EffectfulInstances where

import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import Pure.Domain
import IO.Algebras
import Control.Monad.Random
import Control.Monad.Trans.Random.Lazy
import Data.Functor.Identity

-- newtype TestStack m a = TestStack (S.StateT GameState m a)
--   deriving (Functor, Applicative, Monad)

-- testStack :: TestStack TestConsole ()
-- testStack = state (\gs -> ((), gs)) 

newtype TestConsole a = TestConsole (S.State [String] a)
  deriving (Functor, Applicative, Monad)

testConsole :: a -> TestConsole a
testConsole a = return a

-- instance S.MonadTrans TestStack where
--   lift = S.lift

-- instance MonadState GameState (TestStack TestConsole) where
--     get = S.get
--     put = S.put
--     state = S.state

instance MonadState [String] TestConsole where
  get = S.get
  put = S.put
  state = S.state

instance MonadConsole (S.StateT [String] Identity)  where
  putStrLn s = modify $ \st -> s:st

instance RandomGen () where
  next x = (1, ())
  split _ = ((), ())

newtype TestRandom a = TestRandom (Rand () a)
  deriving (Functor, Applicative, Monad)