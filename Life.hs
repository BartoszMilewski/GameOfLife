{-# LANGUAGE DeriveFunctor #-}
module Life where

import Control.Comonad

-- Bidirectional infinite stream

data BiDi a = BiDi [a] [a]
  deriving Functor
  
shiftBck, shiftFwd :: BiDi a -> BiDi a
shiftBck (BiDi (x:xs) ys) = BiDi xs (x:ys)
shiftFwd (BiDi xs (y:ys)) = BiDi (y:xs) ys

getBD :: BiDi a -> a
getBD (BiDi _ (y:_)) = y

setBD :: a -> BiDi a -> BiDi a
setBD a (BiDi xs (_:ys)) = BiDi xs (a:ys)

constStream :: a -> BiDi a
constStream a = BiDi (repeat a) (repeat a)

-- unfold using two generators and two seeds
unfoldBD :: (s -> (s, a)) -> s -> 
            (s -> (s, a)) -> s -> BiDi a
unfoldBD g s g' s' = BiDi l r
  where l = unfoldLst g  s
        r = unfoldLst g' s'

-- unfold an infinite list
unfoldLst :: (s -> (s, a)) -> s -> [a]
unfoldLst g z = 
  let (z', a) = g z
  in a : unfoldLst g z'

-- Infinite grid

newtype Grid a = Grid (BiDi (BiDi a))
  deriving Functor

set :: a -> Grid a -> Grid a
set a (Grid g) = Grid $ setBD newRow g
  where newRow = setBD a $ getBD g

-- Shifting the Grid

data Up = Up
data Dn = Dn
data Lf = Lf
data Rt = Rt

class Shift dir where
  mv :: dir -> Grid a -> Grid a

instance Shift Up where 
  mv dir (Grid g) = Grid $ shiftFwd g
instance Shift Dn where 
  mv dir (Grid g) = Grid $ shiftBck g
instance Shift Lf where 
  mv dir (Grid g) = Grid $ fmap shiftFwd g
instance Shift Rt where 
  mv dir (Grid g) = Grid $ fmap shiftBck g
  
shiftN :: Shift dir => dir -> Int -> Grid a -> Grid a
shiftN dir n g = iterate (mv dir) g !! n

-- Comonad

instance Comonad Grid where
  extract (Grid g) = getBD (getBD g)
  duplicate g = Grid $ unfoldBD (rowsGen Dn) (mv Dn g) 
                                (rowsGen Up) g
    where rowsGen dir g = (mv dir g, (unfoldLn g))
          unfoldLn g = unfoldBD (listGen Rt) (mv Rt g) (listGen Lf) g
          listGen dir g = (mv dir g, g)

data State = Empty | Full 
  deriving Enum

instance Show State where
  show Empty = "."
  show Full  = "o"

-- Counting neighbors

get3, get2, get1 :: Grid a -> [a]
get3 g = [extract (mv Rt g), extract g, extract (mv Lf g)]
get2 g = [extract (mv Rt g), extract (mv Lf g)]
get1 g = [extract g]

neighbors :: Grid a -> [a]
neighbors g = get3 (mv Up g) ++ get2 g ++ get3 (mv Dn g)

countNeighbors :: Grid State -> Int
countNeighbors = sum . fmap fromEnum . neighbors

emptyGrid :: Grid State
emptyGrid = Grid (constStream (constStream Empty))

-- Calculate next state at the current location in the Grid

nextState :: Grid State -> State
nextState grid = 
  let cnt = countNeighbors grid
  in if cnt == 3 then Full 
     else if cnt == 2 then extract grid 
          else Empty


instance Show a => Show (BiDi a) where
  show (BiDi (x1: x2: x3: xs) (y1: y2 : y3 : y4: ys)) = 
     show x3 ++ " " ++ show x2 ++ " " ++ show x1 ++ " " ++ 
     show y1 ++ " " ++ show y2 ++ " " ++ show y3 ++ " " ++ show y4 ++ "\n"
     
instance Show a => Show (Grid a) where
  show (Grid bd) = show bd

-- From strings to Grid

getGrid :: [String] -> Grid State
getGrid rows = 
  let grid = parse rows emptyGrid
      h = length rows
      w = if h > 0 then length (head rows) else 0
  in  shiftN Dn (h `div` 2) $ shiftN Lf w grid

parse :: [String] -> Grid State -> Grid State
parse [] g = g
parse (l:ls) g = parse ls $ mv Up (parseLn l g)

parseLn :: String -> Grid State -> Grid State
parseLn s g = 
    let (g', n) = go s (g, 0)
    in shiftN Rt n g'
  where go :: String -> (Grid State, Int) -> (Grid State, Int)
        go [] (g, n) = (g, n)
        go (c:cs) (g, n) = 
          let g' = if c == '.' then g else set Full g
          in go cs (mv Lf g', n + 1)


lives :: Grid State -> [Grid State]
lives = iterate $ extend nextState 

testLife = do
  let grid = getGrid [".o.", "..o", "ooo", "...", "..."]
  print $ take 9 (lives grid)

main = do 
  testLife
