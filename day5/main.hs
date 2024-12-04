import qualified Data.Map.Strict as M
import Data.List

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = let (l, s') = break (== c) s
              in l : case s' of
                       [] -> []
                       (_:s'') -> splitOn c s''

parse :: String -> ([(Int, Int)], [[Int]])
parse str =
  let [cs, ms] = splitOn "" . lines $ str
      cs' = map ((\x -> case x of [a,b] -> (a,b)) . map read . splitOn '|') cs
      ms' = map (map read . splitOn ',') ms
  in (cs', ms')

constraints = foldl' (\m (k,v) -> M.insertWith (++) k [v] m) M.empty

middle xs = xs !! (length xs `div` 2)

valid _ [x] = True
valid c (x:xs) = all (\y ->  x `M.member` c && y `elem` (c M.! x)) xs && valid c xs

resolve c = sum . map middle . filter (valid c)


sortC c = sortBy (cmp c)
  where cmp c x y
          | x == y = EQ
          | x `M.member` c && y `elem` (c M.! x) = LT
          | otherwise = GT

resolve2 c = sum . map (middle . sortC c) . filter (not . valid c)

main = do
  (c,ms) <- parse <$> readFile "input.txt"
  print . resolve (constraints c) $ ms
  print . resolve2 (constraints c) $ ms
