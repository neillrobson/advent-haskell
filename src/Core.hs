module Core ((:~>) (..), run, runSome, SomeSolution (..)) where

import Control.Monad ((<=<))
import Util (readFileMaybe)

data a :~> b = MkSol
  { sParse :: String -> Maybe a,
    sSolve :: a -> Maybe b,
    sShow :: b -> String
  }

data SomeSolution = forall a b. MkSomeSol (a :~> b)

execSol :: a :~> b -> String -> Maybe String
execSol MkSol {..} = fmap sShow . (sSolve <=< sParse)

run :: FilePath -> a :~> b -> IO ()
run path sol = do
  input <- readFileMaybe path
  case input of
    Nothing -> fail $ "cannot read file at " ++ path
    Just s -> case execSol sol s of
      Nothing -> fail "no solution found"
      Just o -> putStrLn o

runSome :: FilePath -> SomeSolution -> IO ()
runSome path (MkSomeSol sol) = run path sol
