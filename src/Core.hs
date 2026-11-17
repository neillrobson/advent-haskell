module Core (eitherToMaybe, readFileMaybe, (:~>) (..), run) where

import Control.Applicative (Alternative (empty))
import Control.DeepSeq (force)
import Control.Exception (evaluate, tryJust)
import Control.Monad (guard, (<=<))
import System.IO.Error (isDoesNotExistError)

data a :~> b = MkSol
  { sParse :: String -> Maybe a,
    sSolve :: a -> Maybe b,
    sShow :: b -> String
  }

execSol :: a :~> b -> String -> Maybe String
execSol sol = fmap (sShow sol) . (sSolve sol <=< sParse sol)

run :: FilePath -> a :~> b -> IO ()
run path sol = do
  input <- readFileMaybe path
  case input of
    Nothing -> fail $ "cannot read file at " ++ path
    Just s -> case execSol sol s of
      Nothing -> fail "no solution found"
      Just o -> putStrLn o

eitherToMaybe :: (Alternative m) => Either e a -> m a
eitherToMaybe = either (const empty) pure

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe = (traverse (evaluate . force) . eitherToMaybe) <=< tryJust (guard . isDoesNotExistError) . readFile
