module Show where
import Game
import Text.Read
import Data.List

readGame :: String -> Game
readGame s =
    case lines s of
        [lines, claimedSqs, currentPlayer, maxP, size] -> (read lines, read claimedSqs, read currentPlayer, read maxP, read size)
        _ -> error "[incoherent screaming]"

showGame :: Game -> String
showGame (lines, claimedSqs, currentPlayer, maxP, size) = 
  intercalate "\n" [show lines, show claimedSqs, show currentPlayer, show maxP, show size]

--gtyay :: Game
--gtyay = ([((0,1),East),((0,0),South),((1,0),South)],[],1,2,2)
--writeGame gtyay "testyay.txt"

writeGame :: Game -> FilePath -> IO ()
writeGame g fp = writeFile fp (showGame g)

loadGame :: FilePath -> IO Game
loadGame ft = do
    content <- readFile ft
    return $ readGame content


--a is for a finished game
--b is two moves away from a win; 1 can either tie or lose
--c & d are six moves away but that's just how it goes