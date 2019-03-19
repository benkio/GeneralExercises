{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module IO.EffectfulInstances where

import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import Control.Exception
import Pure.Domain
import IO.Algebras
import Control.Monad.Random
import qualified Control.Monad.Except as E
import Control.Monad.Trans.Random.Lazy
import Data.Functor.Identity

newtype TestConsole m a = TestConsole (S.StateT [String] m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runTestConsole :: [String] -> TestConsole m a -> m (a, [String])
runTestConsole stack (TestConsole x) = runStateT x stack

newtype TestRandom a = TestRandom (Rand () a)
  deriving (Functor, Applicative, Monad)

newtype TestError m a = TestError (E.ExceptT IOException m a)
  deriving (Functor, Applicative, Monad)

testConsole :: Monad m => a -> TestConsole m a
testConsole a = return a

instance Monad m => MonadState [String] (TestConsole m) where
  get = S.get
  put = S.put
  state = S.state

instance Monad m => MonadConsole (TestConsole m)  where
  putStrLn s = S.modify $ \st -> s:st

-- instance Monad m => E.MonadError e (TestError m) where
--   throwError = E.throwE
--   catchError = E.catchE

instance RandomGen () where
  next x = (1, ())
  split _ = ((), ())