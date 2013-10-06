module TransformerExampleDefinitions where

import TransformerExampleImplementations
import TransformerExampleBase

class MonadT t where
  lift :: Monad m => m a -> t m a

instance MonadT StateT where
  lift m = StateT $ \s -> do
    a <- m
    return (s,a)

instance MonadT EnvT where
  lift m = EnvT $ \_ -> m

instance MonadT ErrorT where
  lift m = ErrorT $ do
    a <- m
    return $ Right a
  
