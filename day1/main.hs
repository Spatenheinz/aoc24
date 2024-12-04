import Data.List
import qualified Data.Map.Strict as Map
import Data.Function

readLists = unzip . map ((\x -> case x of [a,b] -> (read a, read b)) . words) . lines
resolve = uncurry (zipWith (\x y -> if x > y then x - y else y - x) `on` sort)
resolve2 (l, r) =
  let m = foldl' (\x a -> Map.insertWith (+) a 1 x) Map.empty r
  in map (\x -> case Map.lookup x m of Just a -> x * a; Nothing -> 0) l
main = do
  x <- readLists <$> readFile "input.txt"
  print . sum $ resolve x
  print . sum $ resolve2 x
