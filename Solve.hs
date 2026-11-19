module Solve where
import Game
import Show
import Data.List


whoWillWin :: Game -> Winner
whoWillWin game@(lines, claimedSqs, player, maxP, size) =
  case possibleGames game of
    [] -> callWinner game
    gs -> let possible = (map whoWillWin (gs)) in if Won player `elem` possible then Won player
          else if Tied `elem` possible then Tied else Won ((player `mod` maxP) +1)

possibleGames :: Game -> [Game]
possibleGames g@(lines, sqsP, p, maxp, size) = [makeMove g l | l <- availableMoves]
    where availableMoves = (legalMoves g) \\ lines

--from the persepctive of p (currentPlayer)
-- evaluateGameState :: Game -> PlayerState
-- evaluateGameState g@(lines, sqsP, p, maxp, size)
--     | (fst currentPlayerStats) > (size*size `div` 2) = Winner --this checks if over half of the boxes have been won
--     | (length lines) == 2*size*size+2*size = Tied --this is the number of lines in a game
--     | otherwise = Ongoing
--         where currentPlayerStats = if length (winsPerPerson sqsP) > 0 then (winsPerPerson sqsP) !! (p-1) else (0,0)


testGameC :: Game
testGameC = readGame "[((0,0),East),((0,1),South),((1,0),South),((1,1),East),((2,1),South),((0,2),East)]\n[]\n1\n2\n2"

testGameA :: Game
testGameA = readGame "[((0,0),East),((0,0),South),((0,1),East),((0,1),South),((1,0),East),((1,0),South),((1,1),East),((1,1),South),((2,0),South),((2,1),South),((0,2),East),((1,2),East)]\n[((0,0), 1), ((1,0), 2), ((0,1), 2), ((1,1), 1)]\n1\n2\n2"