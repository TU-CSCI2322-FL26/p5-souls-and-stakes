Module Graveyard where
import Data.Maybe
import Data.List

-- Core Data Types
-- Square-centric

type Game = ([Line], [(Square, Player)], Player, Int)
type Player = Int
type Corner = (Int, Int)
type Square = Corner
type Line = (Corner, Direction)
type Move = Line

data Direction = East | South