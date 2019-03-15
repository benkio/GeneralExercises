{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module IO.EffectfulInstances where

import qualified Control.Monad.State.Lazy as S
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import Control.Exception
import Pure.Domain
import IO.Algebras
import Control.Monad.Random
import Control.Monad.Except
import Control.Monad.Trans.Random.Lazy
import Data.Functor.Identity

newtype TestConsole m a = TestConsole (S.StateT [String] m a)
  deriving (Functor, Applicative, Monad)

testConsole :: Monad m => a -> TestConsole m a
testConsole a = return a

instance Monad m => MonadConsole (S.StateT [String] m)  where
  putStrLn s = modify $ \st -> s:st

instance RandomGen () where
  next x = (1, ())
  split _ = ((), ())

newtype TestRandom a = TestRandom (Rand () a)
  deriving (Functor, Applicative, Monad)