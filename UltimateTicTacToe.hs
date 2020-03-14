-- UltimateTicTacToe.hs -- data types and core logic of the game
module UltimateTicTacToe where

import           Data.Ix    (inRange)
import           Data.List  (intersperse, transpose)
import           Data.Maybe (isJust, isNothing)


-- * Data types

-- |A type representing a reason why a move is not possible
data ReasonCanNotPlay = OutOfBoard
                      | CaseOccupied
                      | ForcedToPlayLocal
                      | BoardAlreadyWon
                      deriving (Eq, Show)

data Mark = X | O
  deriving (Eq, Show)

-- | 3x3 grid, seen as a matrix where indices begin from 0
type LocalBoard = [[Maybe Mark]]

emptyLocalBoard :: LocalBoard
emptyLocalBoard = replicate 3 $ replicate 3 Nothing

-- | the big 3x3 grid composed of 9 3x3 grids,
-- | seen as a matrix where indices begin from 0
type GlobalBoard = [[LocalBoard]]

emptyGlobalBoard :: GlobalBoard
emptyGlobalBoard = replicate 3 $ replicate 3 emptyLocalBoard

-- | a position on a board (local or global)
type Pos = (Int, Int)


-- * Basic functions (mostly about getting board state info)

-- | determine the winner of a local board
winnerLocal :: LocalBoard -> Maybe Mark
winnerLocal xs
  | [Just X, Just X, Just X] `elem` ys = Just X
  | [Just O, Just O, Just O] `elem` ys = Just O
  | otherwise = Nothing
  where ys = rows ++ columns ++ diagonals
        rows = xs
        columns = transpose xs
        diagonals = [ xs !! 0 !! 0 : xs !! 1 !! 1 : [xs !! 2 !! 2]
                    , xs !! 0 !! 2 : xs !! 1 !! 1 : [xs !! 2 !! 0] ]

-- | determine the winner of the global board
winnerGlobal :: GlobalBoard -> Maybe Mark
winnerGlobal xs = winnerLocal $ map (map winnerLocal) xs

isATie :: GlobalBoard -> Bool
isATie globalBoard = all canNotPlayLocal $ concat globalBoard
  where canNotPlayLocal localBoard =
          hasWinnerLocal localBoard || all isJust (concat localBoard)

outOfBoard :: Pos -> Bool
outOfBoard = not . inRange ((0, 0), (2, 2))

isOccupiedLocal :: Pos -> LocalBoard -> Bool
isOccupiedLocal (x, y) xs = isJust $ xs !! x !! y

isOccupiedGlobal :: Pos -> Pos -> GlobalBoard -> Bool
isOccupiedGlobal (x, y) p xs = isOccupiedLocal p $ xs !! x !! y

hasWinnerLocal :: LocalBoard -> Bool
hasWinnerLocal = isJust . winnerLocal

hasWinnerGlobal :: GlobalBoard -> Bool
hasWinnerGlobal = isJust . winnerGlobal

-- | whether someone can play on the local board or not
canPlayLocal :: LocalBoard -> Bool
canPlayLocal xs = boardNotFull && noWinner
  where boardNotFull = any isNothing (concat xs)
        noWinner = isNothing (winnerLocal xs)


-- * Game functions (game logic)

playLocal :: Pos -> Mark -> LocalBoard
          -> Either ReasonCanNotPlay LocalBoard
playLocal p@(x, y) m xs
  | outOfBoard p = Left OutOfBoard
  | isOccupiedLocal p xs = Left CaseOccupied
  | hasWinnerLocal xs = Left BoardAlreadyWon
  | otherwise =
      Right $ take x xs ++
              [take y (xs !! x) ++ [Just m] ++  drop (y+1) (xs !! x)] ++
              drop (x+1) xs

-- | I had to split the function in two cases, whether you are forced to play somewhere or not
-- | Sorry for this overly confusing piece of code
playGlobal :: Pos -> Pos -> Maybe Pos -> Mark -> GlobalBoard
           -> Either ReasonCanNotPlay GlobalBoard
playGlobal globalPos@(x, y) localPos Nothing m xs
  | outOfBoard globalPos = Left OutOfBoard
  | otherwise = fmap (\localBoard -> take x xs ++
                                     [take y (xs !! x) ++ [localBoard] ++  drop (y+1) (xs !! x)] ++
                                     drop (x+1) xs)
                (playLocal localPos m (xs !! x !! y))
playGlobal globalPos@(x, y) localPos (Just imposedPos@(a, b)) m xs
  | outOfBoard globalPos = Left OutOfBoard
  | globalPos /= imposedPos && canPlayLocal (xs !! a !! b)  = Left ForcedToPlayLocal
  | otherwise = fmap (\localBoard -> take x xs ++
                                     [take y (xs !! x) ++ [localBoard] ++  drop (y+1) (xs !! x)] ++
                                     drop (x+1) xs)
                (playLocal localPos m (xs !! x !! y))


-- * Basic printing functions

-- | human readable representation of a local board
showLocalBoard :: LocalBoard -> String
showLocalBoard xs = unlines $
                    intersperse "---+---+---" $
                    map (concat .
                         intersperse "|" .
                         map (\[x] -> ' ':x:" ") .
                         map (maybe " " show)) xs

-- | print side to side two blocks of text
showSided :: String -> String -> String
showSided xs ys = unlines $ zipWith (++) (lines xs) (lines ys)

-- | human readable representation of a global board
showGlobalBoard :: GlobalBoard -> String
showGlobalBoard = concat .
                  intersperse "===========++===========++===========\n" .
                  map (foldl showSided (repeat '\n') .
                       intersperse "||\n||\n||\n||\n||\n" .
                       map showLocalBoard)
