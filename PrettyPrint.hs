module PrettyPrint where
    import Game
    import Data.Maybe
    import Data.List

    prettyPrint :: Game -> IO ()
    prettyPrint game = putStrLn $ stringSummoningRitual (0,0) game 

    prettyString :: Game -> String
    prettyString game = stringSummoningRitual (0,0) game 


    stringSummoningRitual :: Corner -> Game -> String
    --stringSummoningRitual  (lines, a, b, c, size) = ""
    stringSummoningRitual (x,0) (lines, a, b, c, size)
        | (x==size) = ".\n" ++ (stringSummoningRitual (0,1) (lines, a, b, c, size))
        | (otherwise) = "." ++ (underscoreCheck (x,0) lines a) ++ (stringSummoningRitual (x+1,0) (lines, a, b, c, size))  
    stringSummoningRitual (x,y) (lines, a, b, c, size) = 
        if (x== size) 
            then if (y<size)
            then (excCheck (x,y) lines) ++ "\n" ++ (stringSummoningRitual (0,y+1) (lines, a, b, c, size))
            else (excCheck (x,y) lines)
            else (excCheck (x,y) lines) ++ (underscoreCheck (x,y) lines a) ++ (stringSummoningRitual (x+1,y) (lines, a, b, c, size))

    underscoreCheck :: Corner -> [Line] -> [(Square, Player)] -> String
    underscoreCheck (x,y) lines claimedsquares = 
        let claim = lookup (x,y-1) claimedsquares
        in
            if (claim == Nothing)
            then
                if (((x,y),East) `elem` lines) 
                then underline " "
                else " "
            else underline (show (fromJust claim))

    excCheck :: Corner -> [Line] -> String
    excCheck (x,y) lines = 
        if (((x,y-1), South) `elem` lines)
            then "!"
            else "."

    underline :: String -> String
    underline str = "\ESC[4m" ++ str ++ "\ESC[24m"

    {- Examples:
  . . .     size 2, no moves
  . . .
  . . .

  ._._.     issue: we only keep the dots on the top row.
  |_|_|      Potential solutions: A) add more spaces
  |_|_|                           B) just deal with it

  .__.__.
  |  |  |
  .__.__.     see this looks bad

  just deal with it solution: if (((x,y-1),South) `elem` lines) then append "|"  else append "."

  wait... !.

  ._._.
  !_._!
  !_. .   I do think that looks a little better


  Okay so we're going to recurse on a list of coords that goes down rows in order. There will be <size> dots/!s and 
  <size-1> spaces/_s. -}