import System.Random
import Control.Monad (replicateM)
import Data.Array (array, Array, (!))

-- Cool game idea:
--      players can create a certain number of cells per time period
--      objective is to command the "board"
--      players can spend some in game currency to create, or vote on new "rules"
--      rules might expire?


width  = 40
height = 40
         
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
cellAt (World cells _) loc = cells ! loc

mooreNeighbors = [(-1,-1), (0,-1), (1,-1),
                  (-1, 0),         (1, 0),
                  (-1, 1), (0, 1), (1, 1)]

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) mooreNeighbors

boundedNeighbors :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
boundedNeighbors bounds point = map (wrap bounds) (neighbors point)


count :: Eq a => a -> [a] -> Int
count x xs = length (filter (x==) xs)

neighborsStates :: World CellState -> (Int,Int) -> [CellState]
neighborsStates w (x,y) = map (\loc -> cellAt w loc) (boundedNeighbors (worldBounds w) (x,y))

conwaysRules :: CellState -> Int -> CellState
conwaysRules Dead n | n == 3 = Alive
conwaysRules Alive n | n == 2 || n == 3 = Alive
conwaysRules _ _ = Dead


                   
step :: World CellState -> World CellState
step world@(World cells (w,h)) = World cells' (w,h)
    where
      cells' = reshape w h [conwaysRules (cells!(x,y)) (count Alive (neighborsStates world (x, y))) | x<-[0..w-1], y<-[0..h-1]]

renderCell :: CellState -> String
renderCell Alive = "#"
renderCell Dead  = "."
      
renderWorld :: World CellState -> String
renderWorld (World cells (w,h)) = foldl (++) "" [(if y==0 then "\n" else "") ++ (renderCell (cells!(x,y))) | x<-[0..w-1], y<-[0..h-1]] 

start = do
    a <- randomWorld width height
    return a


loop :: World CellState -> IO ()
loop w = go w
    where go world = do
            line <- getLine
            putStrLn (renderWorld world)
            case line of
              "q" -> return ()
              "10" -> go (skip world 10) -- note: this won't print until 2 executions from the request
              "100" -> go (skip world 100)
              "1000" -> go (skip world 1000) -- note: this slows things down immensely! indeterminately more than 10x "100"
              _ -> go (step world)
          skip world n = foldl (\c _ -> step c) world [0..n] 

main = do
  a <- start
  loop a
  return "end"
