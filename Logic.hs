{-# LANGUAGE TemplateHaskell #-}
module Logic where

import Control.Lens
import Linear.V2 (V2(..), _x, _y)
import Control.Monad.Random



--------------- Model -------------------



type Point = (Int, Int)

--Main elements in the game
data Element = Path | Door | Wanderer deriving (Show)
--Model for directional inputs
newtype Direction = Direction {getVector :: Point} deriving (Show)

width = 78 :: Int -- range [0,79]
height = 22 :: Int -- range [0,23]

-- Directions

north :: Direction
north = Direction (0,-1)

south :: Direction
south = Direction (0,1)

west :: Direction
west = Direction (-1,0)

east :: Direction
east = Direction (1,0)

type World = [[(Point, Element)]]

-- Game state
data Game = Game
  { _wanderer  :: Point        -- ^ player 
  , _score     :: Int          -- ^ score
  , _door :: Point
  , _world     :: World        -- ^ world
  } deriving (Show) 
makeLenses ''Game


--Logic for inputs: either a movement or ESC for exiting
data Input = Move Direction | Escape



--------------- Operations -------------------



-- Vector addition
(<+>) :: (Int,Int)->(Int,Int)->(Int,Int)
(<+>) (a,b) (c,d) = (a+c,b+d)

-- Applies a function to a pair
mapTuple :: (Num a, Num b) => (a->b) -> (a,a) -> (b,b)
mapTuple f (x,y) = (f x, f y) 

-- Applies a different function for each element in a pair
applyOnEach :: (Num a, Num b) => (a->b) -> (a->b) -> (a,a) -> (b,b)
applyOnEach f g (x,y) = (f x, g y)

-- Point comparison
pcomp :: (Int,Int)->(Int,Int)-> Bool
pcomp (a,b) (c,d) = case (compare a c, compare b d) of
                         (EQ,EQ) -> True
                         (_,_) -> False 

--Produces a random number from 1 to n-1
randNumber :: (RandomGen g) => Int -> Rand g Int
randNumber n = getRandomR (1,n)



--------------- Game Logic -------------------



initWorld :: Point -> Point -> Point -> Point -> World
initWorld w d (xmin, ymin) (xmax,ymax) = [[((x,y),Path)|x <- [xmin.. xmax]] | y <- [ymin.. ymax]]

update :: Direction -> Game -> Game
update (Direction d) game = game & wanderer %~ applyOnEach (min width) (min height) . (mapTuple (max 1)) . (<+> d)

reinit :: Game -> Game
reinit game = game & wanderer .~ (0,0) & door .~ (10,10)

initGame :: IO(Game)
initGame = do
   wx <- evalRandIO $ randNumber width
   wy <- evalRandIO $ randNumber height
   dx <- evalRandIO $ randNumber width
   dy <- evalRandIO $ randNumber height
   return $ Game (wx,wy) 0 (dx,dy) $ initWorld (wx,wy) (dx,dy) (0,0) (width,height)


point2Vector :: Point -> V2 Int
point2Vector (x,y) = V2 x y 

checkEdgeCase :: Game -> Game -> Game 
checkEdgeCase old (Game (x,y) s d world) 
  | (x*y) == ((width+1)*height) = old
  | otherwise = Game (x,y) s d world