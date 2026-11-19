module Show where
import Game
import Text.Read1


readGame :: String -> Game
readGame s =
    case lines s of
        [lines, claimedSqs, currentPlayer, maxP, size] -> (read lines, read claimedSqs, read currentPlayer, read maxP, read size)
        _ -> error "[incoherent screaming]"
    


--a is for a finished game
--b is two moves away from a win; 1 can either tie or lose
--c & d are six moves away but that's just how it goes