{-# language TypeFamilies #-}
module Life where
import Data.List
import Control.Comonad
import Data.Distributive
import Data.Functor.Compose
import Data.Functor.Foldable

-- Infinite stream
data Stream a = (:>) { headS :: a
                     , tailS :: Stream a}
  deriving Functor

infixr 5 :>

-- Stream is both a recursive and a corecursive data structure
-- We can fold and unfold it using cata and ana

data Pair a x = P a x
  deriving Functor

type instance Base (Stream a) = Pair a

instance Recursive (Stream a) where
    project (a :> as) = P a as

instance Corecursive (Stream a) where
    embed (P a as) = a :> as

-- Stream is distributive over any functor
instance Distributive Stream where
    distribute :: Functor f => f (Stream a) -> Stream (f a)
    -- distribute stms = (headS <$> stms) :> distribute (tailS <$> stms)
    -- or, using corecursion:
    distribute = ana (\fStms -> P (headS <$> fStms) (tailS <$> fStms))

instance Show a => Show (Stream a) where
  show = unwords . fmap show . take 6 . toInfList

repeatS :: a -> Stream a
repeatS = ana (\a -> P a a)

iterateS :: (a -> a) -> a -> Stream a
iterateS f = ana (\a -> P a (f a))

-- The first argument is the padding
fromListS :: a -> [a] -> Stream a
fromListS z = ana go
  where go [] = P z []
        go (a : as) = P a as

toInfList :: Stream a -> [a]
toInfList = cata (\(P a as) -> a : as)

-- Bidirectional infinite stream
-- Contains a backward and a forward stream
data Cursor a = Cur { bwStm :: Stream a
                    , fwStm :: Stream a }
  deriving Functor

instance Distributive Cursor where
    distribute :: Functor f => f (Cursor a) -> Cursor (f a)
    distribute fCur = Cur (distribute (bwStm <$> fCur)) 
                          (distribute (fwStm <$> fCur))

instance Comonad Cursor where
  extract (Cur _ (a :> _)) = a
  duplicate bi = Cur (iterateS moveBwd (moveBwd bi)) 
                     (iterateS moveFwd bi)

instance Show a => Show (Cursor a) where
  show (Cur _ fw) = show fw ++ "\n"

moveFwd :: Cursor a -> Cursor a
moveFwd (Cur bw (a :> as)) = Cur (a :> bw) as

moveBwd :: Cursor a -> Cursor a
moveBwd (Cur (a :> as) fw) = Cur as (a :> fw)

repeatCur :: a -> Cursor a
repeatCur a = Cur (repeatS a) (repeatS a)

listToCur :: a -> [a] -> Cursor a
listToCur z as = Cur (repeatS z) (fromListS z as)

get2 :: Cursor a -> [a]
get2 cur = [extract (moveBwd cur), extract (moveFwd cur)]

get3 :: Cursor a -> [a]
get3 cur = [extract (moveBwd cur), extract cur, extract (moveFwd cur)]


data Cell = Empty | Full
  deriving Enum

instance Show Cell where
  show Empty = "."
  show Full  = "o"

-- 2-dimensional infinite grid
-- A bidirectional stream of bidirectional streams

type Grid a = Compose Cursor Cursor a

instance (Comonad w2, Comonad w1, Distributive w1) => Comonad (Compose w2 w1) where
    extract = extract . extract . getCompose
    duplicate = fmap Compose . Compose .
                fmap distribute . duplicate . fmap duplicate .
                getCompose

instance {-# OVERLAPPING #-} Show a => Show (Grid a) where
  show = show . getCompose

matrixToGrid :: a -> [[a]] -> Grid a
matrixToGrid z = Compose . listToCur (repeatCur z) . fmap (listToCur z)

get8neighbors :: Grid a -> [a]
get8neighbors (Compose grid) =
  get3 (extract $ moveBwd grid) ++
  get2 (extract grid) ++
  get3 (extract $ moveFwd grid)

countNeighbors :: Grid Cell -> Int
countNeighbors = sum . fmap fromEnum . get8neighbors

-- Calculate next generation at the current location in the Grid

nextGen :: Grid Cell -> Cell
nextGen grid
  | cnt == 3 = Full
  | cnt == 2 = extract grid
  | otherwise = Empty
  where
      cnt = countNeighbors grid

generations :: Grid Cell -> [Grid Cell]
generations = iterate $ extend nextGen

parseChar :: Char -> Cell
parseChar '.' = Empty
parseChar 'o' = Full
parseChar _ = error "Invalid grid input"

main :: IO ()
main = do
  -- A glider pattern
  let matrix = fmap (fmap parseChar) [".o.", "..o", "ooo", "...", "..."]
      grid = matrixToGrid Empty matrix
  print $ take 9 (generations grid)
