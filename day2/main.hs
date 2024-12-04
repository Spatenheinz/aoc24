import Debug.Trace

readLists = map (map read . words) . lines

isSafe = go
  where
    go xs = all' (\x -> x > 0 && x < 4) xs || all' (\x -> x < 0 && x > -4) xs
    all' f = all f . (zipWith (-) <*> tail)

perm _ [] = [[]]
perm 0 xs = [xs]
perm 1 (x : xs) = xs : map (x :) (perm 1 xs)

resolve lvl = length . filter (any isSafe . perm lvl)

main = do
  x <- readLists <$> readFile "input.txt"
  print . resolve 0 $ x
  print . resolve 1 $ x
