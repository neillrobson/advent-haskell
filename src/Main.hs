{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (session, main) where

import Advent (AoC (AoCPrompt), AoCUserAgent (AoCUserAgent), Part (Part1), defaultAoCOpts, mkDay_, runAoC)
import Control.Exception (tryJust)
import Control.Monad (guard)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Map ((!))
import Data.Text (unpack)
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

newtype Config = Cfg {_cfgSession :: Maybe String} deriving (Generic)

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

session :: FilePath -> IO (Maybe String)
session = fmap _cfgSession . configFile

--------------------------------------------------------------------------------

aocUserAgent :: AoCUserAgent
aocUserAgent = AoCUserAgent "github.com/neillrobson/advent-haskell" "neill@neillrobson.com"

main :: IO ()
main = do
  key <- session defaultConfPath
  case key of
    Nothing -> putStrLn "No key given"
    Just k -> do
      let aocOpts = defaultAoCOpts aocUserAgent 2015 k
      prompt <- runAoC aocOpts $ AoCPrompt (mkDay_ 1)
      case prompt of
        Left err -> print err
        Right pMap -> putStrLn $ unpack $ pMap ! Part1
