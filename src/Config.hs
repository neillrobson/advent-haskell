{- HLINT ignore "Use newtype instead of data" -}
module Config (configFile, defConfPath) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Default (Default (..))
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

data Config = Cfg
  { _cfgSession :: Maybe String
  }
  deriving (Generic)

instance Default Config where
  def :: Config
  def = Cfg {_cfgSession = Nothing}

-- Transform _cfgSessionKey to session-key
configJSON :: A.Options
configJSON = A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '-' . drop 4}

instance A.ToJSON Config where
  toJSON = A.genericToJSON configJSON

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON configJSON

defConfPath :: FilePath
defConfPath = "aoc-conf.yaml"

configFile :: FilePath -> IO Config
configFile fp = do
  input <- tryJust (guard . isDoesNotExistError) $ BS.readFile fp
  case input of
    Left () -> do
      Y.encodeFile @Config fp def
      return def
    Right s -> do
      case Y.decodeEither' s of
        Left e -> do
          printf "Config file at %s could not be parsed: " fp
          print e
          return def
        Right cfg -> return cfg
