Module Graveyard where
import Data.Maybe
import Data.List

-- Core Data Types
-- Square-centric

type Game = ([Line], [(Square, Player)], Player, Int, Size)
{-
data Game = Game { lines :: [Line], squares :: [(Square, Player)], turn :: Player, numPlayers :: Int, size :: Int {-or (Int, Int) -} }
newGame numPlayers size =  Game [] [] 0 numPlayers size
gameWithNextPlayer aGame = aGame { turn = turn aGame + 1}
gameWithNextPlayer (lines, squares, turn, numPayers, size) = (lines, squares, turn+1, numPlayers, size) 
-}
type Size = Int
type Player = Int
type Corner = (Int, Int)
type Square = Corner - bottomr ight corner
type Line = (Corner, Direction)

data Direction = East | South deriving (Show, Eq)
