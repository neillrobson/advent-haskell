-- | Loading challenge data and prompts from the Advent of Code website.
module Core.Load (ChallengeSpec (..), ChallengePaths (..), ChallengeData (..), challengeData) where

import Advent
import Control.Applicative (asum)
import Control.Exception (tryJust)
import Control.Monad (guard)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

aocUserAgent :: AoCUserAgent
aocUserAgent = AoCUserAgent "github.com/neillrobson/advent-haskell" "neill@neillrobson.com"

type ExceptionOr = Either [String]

data ChallengeSpec
  = CS
  { _csDay :: Day,
    _csPart :: Part
  }

-- | A record of filesystem locations where challenge data is stored.
data ChallengePaths
  = CP
  { _cpPrompt :: FilePath,
    _cpInput :: FilePath
  }

-- | A record of data associated with a given challenge.
data ChallengeData
  = CD
  { _cdPrompt :: ExceptionOr String,
    _cdInput :: ExceptionOr String
  }

challengePaths :: Integer -> ChallengeSpec -> ChallengePaths
challengePaths y (CS d p) =
  CP
    { _cpPrompt = printf "prompt/%04d/%02d%c.md" y d' p',
      _cpInput = printf "data/%04d/%02d%c.txt" y d' p'
    }
  where
    d' = dayInt d
    p' = partChar p

makeChallengeDirs :: ChallengePaths -> IO ()
makeChallengeDirs CP {..} = mapM_ (createDirectoryIfMissing True . takeDirectory) [_cpPrompt, _cpInput]

--------------------------------------------------------------------------------

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b Nothing = Left b
maybeToEither _ (Just a) = Right a

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe = fmap eitherToMaybe . tryJust (guard . isDoesNotExistError) . readFile

--------------------------------------------------------------------------------

-- | Pull challenge data from either online or local cache.
challengeData ::
  Maybe String ->
  Integer ->
  ChallengeSpec ->
  IO ChallengeData
challengeData sess year spec = do
  makeChallengeDirs paths
  prompt <- asum [maybeToEither [printf "Input file not found at %s" _cpInput] <$> readFileMaybe _cpInput]
  return
    CD
      { _cdPrompt = prompt,
        _cdInput = undefined
      }
  where
    paths@CP {..} = challengePaths year spec
