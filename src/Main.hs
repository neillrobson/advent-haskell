module Main (main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import System.IO.Error (isDoesNotExistError)

-- data Config = Cfg { _cfgSession :: Maybe String }

defaultConfPath :: FilePath
defaultConfPath = "aoc-conf.yaml"

-- configFile :: FilePath -> IO Config
-- configFile fp = do
--   cfgInp <- tryJust (guard . isDoesNotExistError) $ BS.readFile fp
--   return cfgInp

main :: IO ()
main = do
  cfgInp <- tryJust (guard . isDoesNotExistError) $ BS.readFile defaultConfPath
  case cfgInp of
    Left () ->
      putStrLn "File does not exist"
    Right b ->
      BS.putStr b
