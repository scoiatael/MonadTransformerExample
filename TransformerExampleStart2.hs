module TransformerExampleStart2 where

import System.Environment(getArgs)
import TransformerExampleBase
import TransformerExample2

usage = " provide: <funcPath> <termPath> \n"

main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args then putStr usage else contMain args

contMain :: [String] -> IO ()
contMain args = do
  (funcPath, termPath) <- aux $ case length args of 
    0 -> (readPath "Function", readPath "Term to be evaluated") 
    1 -> (return $ args !! 0, readPath "Term to be evaluated")
    2 -> (return $ args !! 0, return $ args !! 1)
    _ -> error "Too many args"
  func <- readFile funcPath
  term <- readFile termPath
  putStrLn $ start $ runInterpreter func term 
  where
  runInterpreter :: String -> String -> OurMonad Value
  runInterpreter func term = do
    menv <- parseEnv $ func
    env <- case menv of 
      Nothing -> err $ "Error parsing functions {\n " ++ func ++ "\n}"
      Just x -> return x
    let mt = maybeTerm term
    t <- case mt of
      Nothing -> err $ "Error parsing term {\n" ++ term ++ "\n}"
      Just x -> return x
    inEnv env $ interp t
  aux :: (IO a, IO b) -> IO (a,b)
  aux (a,b) = do
    a' <- a
    b' <- b
    return (a',b')
  readPath str = do
    putStrLn $ "Path to " ++ str ++ " not found, provide other: "
    getLine
