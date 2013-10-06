module TransformerExample1 where

import TransformerExampleBase
import Data.Maybe(fromJust)

newtype StateC a = StateC { runStateC:: State -> (State, a) }

instance Monad StateC where
  return x = StateC $ \s -> (s,x) 
  m >>= f = StateC $ (\s -> let (s',a) = runStateC m $ s in runStateC (f a) $ s')

instance MonadState StateC where
  modState f = StateC $ \s -> (f s, s)
  putVar v t = modState (putState v t) >> return ()
  getVar v = StateC $ \s -> (s, lookup v s)

newtype EnvC a = EnvC { runEnvC :: Env -> a }

instance Monad EnvC where
  return x = EnvC $ \_ -> x
  m >>= f = EnvC $ \r -> runEnvC (f (runEnvC m $ r)) $ r

instance MonadEnv EnvC where
  parseEnv = return.strToEnv   
  inEnv e f = return $ runEnvC f e
  lookupEnv f = EnvC $ \r -> lookup f r 

newtype ErrorC a = ErrorC { runErrorC :: Either Error a }

instance Monad ErrorC where
  return x = ErrorC $ Right x
  m >>= f = case runErrorC m of
    Left e -> ErrorC $ Left e
    Right a -> f a

instance MonadError ErrorC where
  err e = ErrorC $ Left e

newtype OurMonad a = OurMonad { runO:: EnvC (StateC a) }
-- EnvC (Env -> StateC (State -> (State, a)))

instance Monad OurMonad where
  return x = OurMonad $ return $ return x
  m >>= f = OurMonad $ EnvC ( \r -> runEnvC (runO m) r >>= (\a -> runEnvC (runO (f a)) r))

instance MonadState OurMonad where
  modState f = OurMonad $ return $ modState f 
  putVar v t  = OurMonad $ return $ putVar v t 
  getVar v = OurMonad $ return $ getVar v

instance MonadEnv OurMonad where
  parseEnv s = return $ strToEnv s
  inEnv e f = OurMonad $ return $ runEnvC (runO f) e
  lookupEnv f = OurMonad $ EnvC $ \r -> return $ runEnvC (lookupEnv f) r

instance MonadError OurMonad where
  err = error

discardState = snd

instance InterpMonad OurMonad where
  start o = show $ discardState $ runStateC (runEnvC (runO o) $ basicEnv) $ basicState


