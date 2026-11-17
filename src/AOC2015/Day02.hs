module AOC2015.Day02 () where

--------------------------------------------------------------------------------

parseDimension :: String -> Maybe [Integer]
parseDimension s = do
  let dims = splitBy (== 'x') s
  return []

splitBy :: (Char -> Bool) -> String -> [String]
splitBy test s = case dropWhile test s of
  "" -> []
  s' -> w : splitBy test ws
    where
      (w, ws) = break test s'

--------------------------------------------------------------------------------

wrapBoxes :: [[Integer]] -> Integer
wrapBoxes = sum . fmap wrapBox

wrapBox :: [Integer] -> Integer
wrapBox lwh =
  let areas = product <$> subsets 2 lwh
   in minimum areas + 2 * sum areas

subsets :: Integer -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = ((x :) <$> subsets (n - 1) xs) ++ subsets n xs
