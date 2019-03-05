module IO.Algebras where

class MonadConsole m where
  putStrLn :: String -> m ()

instance MonadConsole IO where
  putStrLn = Prelude.putStrLn
