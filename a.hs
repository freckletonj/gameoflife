import System.Random
import Control.Monad (replicateM)
import Data.Array (array, Array, (!))
    
-- World
--   size
--   topology (torus)
--   world shape (squares? hex?)
--   setting cells (side-effectually, as well)
--
-- Transition Fn
--   deterministic (later, stochastic)
--   neighbor counts only (later, more complex, eg neighbor's class)
--


width  = 4
height = 4

data CellState = Alive | Dead deriving (Show, Eq)
    
randomCell :: IO CellState
randomCell = do
  x <- randomIO
  return (if x then Alive else Dead)


reshape :: Int -> Int -> [a] -> Array (Int, Int) a
reshape w h xs = grid
    where
      locs = [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]]
      grid = array ((0,0), ((w-1),(h-1))) (zip locs xs)

-- assumes a Toroidal topology
wrap :: (Int,Int) -> (Int,Int) -> (Int,Int)
wrap (w,h) (x,y) = ((mod x w), (mod y h))
      
data World a = World {
      worldCells :: (Array (Int,Int) a),
      worldBounds :: (Int, Int)
    } deriving (Show)

             
randomWorld :: Int -> Int -> IO (World CellState)
randomWorld w h = do
  cells <- replicateM (w * h) randomCell
  return (World (reshape w h cells) (w,h))

cellAt :: World a -> (Int, Int) -> a
cellAt (World xs _) loc = xs ! loc

mooreNeighbors = [(-1,-1), (0,-1), (1,-1),
                  (-1, 0),         (1, 0),
                  (-1, 1), (0, 1), (1, 1)]

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) mooreNeighbors

boundedNeighbors :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
boundedNeighbors bounds point = map (wrap bounds) (neighbors point)



--countLivingNeighbors :: World a -> (Int,Int)

transition :: World a -> World a
transition = undefined


--
w = (randomWorld width height) 
main = do
  w >>= print
