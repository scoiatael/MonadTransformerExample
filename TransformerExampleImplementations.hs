module TransformerExampleImplementations where
import TransformerExampleBase
import System.Environment(getArgs)
import Data.Maybe (fromJust)

newtype StateT m a = StateT { runStateT :: State -> m (State,a) }
instance Monad m => Monad (StateT m) where
  return x = StateT $ \s -> return (s,x)
  m >>= f = StateT $ \s -> do
    (s',a) <- runStateT m s
    runStateT (f a) s'

instance Monad m => MonadState (StateT m) where
  modState f = StateT $ \s -> return (f s, s)
  putVar v t = modState (putState v t) >> return ()
  getVar v = StateT $ \s -> return (s, lookup v s)

newtype EnvT m a = EnvT { runEnvT :: Env -> m a }
instance Monad m => Monad (EnvT m) where
  return x = EnvT $ \_ -> return x
  m >>= f = EnvT $ \e -> do
    a <- runEnvT m e
    runEnvT (f a) e

instance Monad m => MonadEnv (EnvT m) where
  parseEnv =  return.strToEnv
  inEnv e f = EnvT $ \_ -> runEnvT f e
  lookupEnv f = EnvT $ \e -> return $ lookup f e

newtype ErrorT m a = ErrorT { runErrorT :: m (Either Error a) }
instance Monad m => Monad (ErrorT m) where
  return x = ErrorT $ return $ Right x
  m >>= f = ErrorT $ do
    merr <- runErrorT m
    case merr of 
      Left err -> return $ Left err
      Right a -> runErrorT (f a)

instance Monad m => MonadError (ErrorT m) where
  err e = ErrorT $ return $ Left e
