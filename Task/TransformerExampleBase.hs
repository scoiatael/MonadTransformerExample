module TransformerExampleBase where

import Data.List (insert,delete)
import Data.Maybe (fromJust)
import Control.Monad (sequence)

type Variable = String
type Value = Bool
type Function = String

data Term = And Term Term | Or Term Term 
  | Var Variable | Not Term 
  | Let Variable Term Term | Const Value
  | Twofold Variable Term  deriving (Read, Show, Ord, Eq)

maybeTerm :: String -> Maybe Term
maybeTerm s = case reads s of
  [(x,_)] -> Just x
  _        -> Nothing

class (MonadState m, MonadWriter m, MonadList m, MonadError m) => InterpMonad m where
  start :: (Show a, InterpMonad m) => m a -> String

interp :: InterpMonad m => Term -> m Value
interp x = case x of
  And t1 t2 -> do
    v1 <- interp t1
    tell $ "First 'and' val: " ++ show v1 ++ " ."
    case v1 of
      False -> return False
      True -> do
        v2 <- interp t2
        return v2
  Or t1 t2 -> do
    v1 <- interp t1
    tell $ "First 'or' val: " ++ show v1 ++ " ."
    case v1 of 
      True -> return True
      False -> do
        v2 <- interp t2
        return v2
  Not t -> withNoStateChange $ do
    v <- interp t
    return (not v) 
  Var v -> do
    mv' <- getVar v
    if mv' ==  Nothing 
      then err $ "No such variable as " ++ v ++ " at this point."
      else interp $ fromJust mv'
  Let v t t' -> withNoStateChange $ do
    mv' <- getVar v
    if mv' /= Nothing then tell ("Warning: overwriting var " ++ v ++ " .") else return ()
    v' <- interp t
    if mv' /= Nothing then tell (" old val " ++ show (fromJust mv') ++ " new val " ++ show v' ++ " .") else return ()
    putVar v (Const v')
    v'' <- interp t'
    return v'' 
  Const v -> return v
  Twofold v t -> do
    mv <- getVar v
    if mv /= Nothing
      then err $ "Variable " ++ v ++ " already taken at this point."
      else return ()
    let v1 = branch v True t
    let v2 = branch v False t
    merge v1 v2
  where
  branch :: InterpMonad m => Variable -> Value -> Term -> m Value
  branch v val t = withNoStateChange $ do
    tell ("Testing " ++ v ++ " as " ++ show val ++ "..") 
    putVar v (Const val)
    val' <- interp t 
    tell ("Done with " ++ v ++ ".")
    return val'
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

class Monad m => MonadState m where
  modState :: (State -> State) -> m State
  putVar :: Variable -> Term -> m ()
  getVar :: Variable -> m (Maybe Term)

type Log = String
basicLog :: [Log]
basicLog = ["----Logs begin,behold. ----"]

class Monad m => MonadWriter m where
  tell :: String -> m ()

class Monad m => MonadList m where
  merge :: m a -> m a -> m a

type Error = String
class Monad m => MonadError m where
  err :: Error -> m a
