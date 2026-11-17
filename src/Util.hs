module Util (readFileMaybe, stripNewline) where

import Control.Applicative (Alternative (empty))
import Control.DeepSeq (force)
import Control.Exception (evaluate, tryJust)
import Control.Monad (guard, (<=<))
import System.IO.Error (isDoesNotExistError)

stripNewline :: String -> String
stripNewline = reverse . dropWhile (== '\n') . reverse

eitherToMaybe :: (Alternative m) => Either e a -> m a
eitherToMaybe = either (const empty) pure

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe = (traverse (evaluate . force) . eitherToMaybe) <=< tryJust (guard . isDoesNotExistError) . readFile
