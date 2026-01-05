{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

data Config = Cfg {_cfgSession :: Maybe String} deriving (Generic)

configJSON :: A.Options
configJSON =
  A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '-' . drop 4
    }

instance A.ToJSON Config where
  toJSON = A.genericToJSON configJSON
  toEncoding = A.genericToEncoding configJSON

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON configJSON

def :: Config
def = Cfg {_cfgSession = Nothing}

defaultConfPath :: FilePath
defaultConfPath = "aoc-conf.yaml"

configFile :: FilePath -> IO Config
configFile fp = do
  cfgInp <- tryJust (guard . isDoesNotExistError) $ BS.readFile fp
  case cfgInp of
    Left () ->
      return def
    Right b ->
      case Y.decodeEither' b of
        Left e -> do
          printf "Configuration file at %s could not be parsed:\n" fp
          print e
          return def
        Right cfg -> return cfg

main :: IO ()
main = do
  cfg <- configFile defaultConfPath
  case _cfgSession cfg of
    Nothing -> putStrLn "Not found!"
    Just key -> putStrLn key
