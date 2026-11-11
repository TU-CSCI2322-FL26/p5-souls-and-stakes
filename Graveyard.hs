--Module Graveyard where
import Data.Maybe
import Data.List

-- Core Data Types
--story 1: defining data types and whatnot

--lines in the game, mapping from a square to its player, current player, max players, square size
type Game = ([Line], [(Square, Player)], Player, Int, Size)
type Size = Int
type Player = Int
type Corner = (Int, Int)
type Square = Corner --top left corner
type Line = (Corner, Direction)
data Direction = East | South deriving (Show, Eq)

--story 2: call winner
--only triggers when lenght legalMoves == 0
callWinner :: Game -> Player
callWinner (_, filled, _, _, _) = if detectTie playerWins then 100 --deal with ties at some point
                                  else snd (maximum playerWins)
    where playerWins = winsPerPerson filled


winsPerPerson :: [(Square, Player)] -> [(Int, Player)] 
winsPerPerson claimed = map personToSquares playerList
    where playerList = nub [p | (_, p) <- claimed]
          personToSquares x = (length [sq | (sq, pl) <- claimed, pl == x], x)
    

detectTie :: [(Int, Player)] -> Bool
detectTie lst = length (filter (==bestScore) [x | (x, _) <- lst]) > 1
    where bestScore = fst (maximum lst)


--story 3: making a move

{--
ISSUE: Currently, the way we represent the game makes it so we cannot represent the lines that made up the bottom and
right borders of the grid. See text message for visual representation. This, as you can imagine, is bad! Need to speak
to Dr. Fogarty about this soon.

makeMove :: Game -> Line -> Game
makeMove (lines, claimedSqs, currentPlayer, maxP, size) move = ((move:lines), (updateSquares ???), nextPlayer, maxP, size)
    where nextPlayer = (currentPlayer + 1) % maxP

updateSquares :: [(Square, Player)] -> [Line] -> Line -> [(Square, Player)]
updateSquares claimedSquares currentLines newLine = I DUNNO--}




--story 4: computes legal moves
--legal moves must be made of a valid square and a direction + must not already be in the game
legalMoves :: Game -> [Line]
legalMoves (currentLines, _, _, _, size) = [line | line <- possibleLines, not (line `elem` currentLines)]
    where allSquares = [(x, y) | x <- [0..size], y <- [0..size]]
          possibleLines = [(corner, direction) | corner <- allSquares, direction <- [East, South]]

--size of the game is a square, so 3x3 means we have squares (0,0) through (2,2)
--so legal moves would be like ((0,0), East), ((0,0), South), etc; aka moves not owned by anyone