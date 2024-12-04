import qualified Data.Vector as V
import Data.Vector ((!))

readTable = V.map V.fromList . V.fromList

b2i b = if b then 1 else 0
b2i' b f = if b then f else 0
for = flip fmap

mas = V.fromList "MAS"
checkmas x fi fj = b2i $ and $ for [1..3] $ \k -> x ! fi k ! fj k == mas ! (k - 1)

map' x =
  let rlen3 = V.length x - 3
      clen3 = V.length (x ! 0) - 3
  in for [0..V.length x - 1] $ \i -> for [0.. V.length (x ! 0) - 1] $ \j ->
      b2i' ((x ! i) ! j == 'X') $ sum $ map (uncurry b2i') [
          (j > 2, b2i $ V.slice (j - 3) 3 (x ! i)  == V.reverse mas) -- W
        , (j < clen3, b2i $ V.slice (j + 1) 3 (x ! i) == mas)        -- E
        , (i > 2 , checkmas x (i -) (const j))                       -- N
        , (i < rlen3 , checkmas x (i +) (const j))                   -- S
        , (i > 2 && j > 2 , checkmas x (i -) (j -))                  -- NW
        , (i > 2 && j < clen3 , checkmas x (i -) (j +))              -- NE
        , (i < rlen3 && j > 2 , checkmas x (i +) (j -))              -- SW
        , (i < rlen3 && j < clen3 , checkmas x (i +) (j +))          -- SE
      ]
map'2 x =
  let rlen2 = V.length x - 2
      clen2 = V.length (x ! 0) - 2
  in for [1.. rlen2] $ \i -> for [1.. clen2] $ \j ->
      let isA = b2i $ (x ! i) ! j == 'A'
          lu2rd = b2i $ x ! (i - 1) ! (j - 1) == 'M' && x ! (i + 1) ! (j + 1) == 'S'
          rd2lu = b2i $ x ! (i + 1) ! (j + 1) == 'M' && x ! (i - 1) ! (j - 1) == 'S'
          ld2ru = b2i $ x ! (i + 1) ! (j - 1) == 'M' && x ! (i - 1) ! (j + 1) == 'S'
          ru2ld = b2i $ x ! (i - 1) ! (j + 1) == 'M' && x ! (i + 1) ! (j - 1) == 'S'
       in isA * (lu2rd + rd2lu) * (ld2ru + ru2ld)

resolve f = sum . map sum . f

main = do
  x <- (readTable . lines) <$> readFile "input.txt"
  print . resolve map' $ x
  print . resolve map'2 $ x
