module TransformerExampleBase where

import Data.List (insert,delete)
import Data.Maybe (fromJust)
import Control.Monad (sequence)

type Variable = String
type Value = Bool
type Function = String

maybeTerm :: String -> Maybe Term
maybeTerm s = case reads s of
  [(x,_)] -> Just x
  _        -> Nothing

data Term = And Term Term | Or Term Term 
  | Var Variable | Not Term 
  | Let Variable Term Term | Const Value
  | Appl Function Term deriving (Read, Show, Ord, Eq)

data Func = Func Function Variable Term  deriving (Show, Read, Ord, Eq)

class (MonadState m, MonadEnv m, MonadError m) => InterpMonad m where
  start :: (Show a, InterpMonad m) => m a -> String

class Monad m => MonadState m where
  modState :: (State -> State) -> m State
  putVar :: Variable -> Term -> m ()
  getVar :: Variable -> m (Maybe Term)

class Monad m => MonadEnv m where
  parseEnv :: String -> m (Maybe Env)
  inEnv :: Env -> m a -> m a
  lookupEnv :: Function -> m (Maybe Func)

class Monad m => MonadError m where
  err :: Error -> m a

interp :: InterpMonad m => Term -> m Value
interp x = case x of
  And t1 t2 -> do
    v1 <- interp t1
    v2 <- interp t2
    return (v1 && v2)
  Or t1 t2 -> do
    v1 <- interp t1
    v2 <- interp t2
    return (v1 || v2)
  Not t -> withNoStateChange $ do
    v <- interp t
    return (not v) 
  Var v -> do
    mv' <- getVar v
    if mv' ==  Nothing 
      then err $ "No such variable as " ++ v ++ " at this point."
      else interp $ fromJust mv'
  Let v t t' -> withNoStateChange $ do
    v' <- interp t
    putVar v (Const v')
    v'' <- interp t'
    return v'' 
  Const v -> return v
  Appl f t -> withNoStateChange $ do
    v <- interp t
    mf' <- lookupEnv f
    f' <- if mf' == Nothing 
      then err $ "No such function as " ++ f ++ " at this point." 
      else return $ fromJust mf'
    let Func _ var b = f'
    interp $ Let var (Const v) b
  where
  withNoStateChange f = do
    s <- modState id
    a <- f
    modState (\_ -> s) 
    return a
    

type State = [(Variable,Term)]
basicState :: State
basicState=[]
putState :: Variable -> Term -> State -> State
putState v t s = case lookup v s of
  Nothing -> insert (v,t) s 
  Just t' -> insert (v,t) $ delete (v,t') s


type Env = [(Function, Func)]
basicEnv :: Env
basicEnv = []

strToEnv :: String -> Maybe Env
strToEnv s = (fromMaybeList $ map maybeReadFunc $ lines s) >>= (Just . map (\x@(Func n _ _) -> (n,x)))
  where
  fromMaybeList :: [Maybe a] -> Maybe [a]
  fromMaybeList = sequence
  maybeReadFunc str = case reads str of
    [(x,"")] -> Just x
    _      -> Nothing

myParse :: String -> String
myParse = head . filter (\(x:_) -> x /= '#') . lines

--Moze jednak dodac error?

type Error = String

