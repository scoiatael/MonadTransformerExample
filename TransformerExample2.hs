module TransformerExample2 where 

import TransformerExampleBase
import TransformerExampleImplementations
import TransformerExampleDefinitions

import System.Environment(getArgs)

newtype Id a = Id { runId :: a }
instance Monad Id where
  return = Id
  m >>= f = f (runId m)

newtype OurMonad a = OurMonad { runO :: ErrorT (EnvT (StateT (Id))) a } 
liftFromErr :: ErrorT (EnvT (StateT (Id))) a -> OurMonad a
liftFromErr = OurMonad 
liftFromEnv :: EnvT (StateT (Id)) a -> OurMonad a
liftFromEnv = liftFromErr.lift
liftFromState :: StateT (Id) a -> OurMonad a
liftFromState = liftFromEnv.lift

instance Monad OurMonad where
  return = OurMonad . return 
  m >>= f = OurMonad $ do
    a <- runO m
    runO $ f a

instance MonadError OurMonad where
  err str = liftFromErr $ err str

instance MonadState OurMonad where
  modState f = liftFromState $ modState f
  putVar v t = liftFromState $ putVar v t
  getVar v = liftFromState $ getVar v

instance MonadEnv OurMonad where
-- !! Curious case !! --
  inEnv e f = do
    a <- liftFromEnv $ inEnv e $ runErrorT $ runO f 
    case a of
      Left e -> err e
      Right a -> return a
  parseEnv s = liftFromEnv $ parseEnv s
  lookupEnv f = liftFromEnv $ lookupEnv f
 
discardState = snd

instance InterpMonad OurMonad where
  start o = case runId $ runStateT (runEnvT (runErrorT $ runO o) basicEnv) basicState of
    (s, Left err) -> "Error: " ++ err ++ "\n in state: \n" ++ s ++ "--\n"
    (_,Right a) -> show a
