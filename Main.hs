module Main where

import           Data.Char         (ord)
import           Data.Char         (isAlpha, toUpper)
import           System.IO         (hFlush, stdout)
import           Text.Read         (readMaybe)
import           UltimateTicTacToe

-- | type representing the command of a player
type Command = (Char, Int)

-- | type representing the state of the game
data GameState = GS { board            :: GlobalBoard
                    , currentPlayer    :: Mark
                    , maybeNextPos     :: Maybe Pos -- position where the next user should play
                    , maybeLastCommand :: Maybe Command
                    }

beginningGameState :: GameState
beginningGameState = GS emptyGlobalBoard X Nothing Nothing

putGameState :: GameState -> IO ()
putGameState (GS globalBoard _ _ maybeLastCommand) = do
  putStrLn "   A   B   C    D   E   F    G   H   I"
  putStr $ showSided
    " 1\n  \n 2\n  \n 3\n  \n 4\n  \n 5\n  \n 6\n  \n 7\n  \n 8\n  \n 9\n"
    (showGlobalBoard globalBoard)
  case maybeLastCommand of
    Just (c, i) -> putStrLn $ "\nLast move: " ++ [c] ++ show i
    Nothing     -> return ()

readMaybeCommand :: String -> Maybe Command
readMaybeCommand ""       = Nothing
readMaybeCommand (' ':xs) = readMaybeCommand xs
readMaybeCommand (x:xs) = if isAlpha x
                          then case readMaybe xs of
                                 Just n -> Just (toUpper x, n)
                                 _      -> Nothing
                          else Nothing

-- | Convert the human readable command code to positions
commandToPos :: Command -> (Pos, Pos)
commandToPos (c, i) = (p1, p2)
  where p1 = ((i - 1) `div` 3, (ord c - ord 'A') `div` 3)
        p2 = ((i - 1) `mod` 3, (ord c - ord 'A') `mod` 3)

play :: GameState -> IO ()
play gameState@(GS globalBoard currentPlayer maybeNextPos _) = do
  -- show the game board and prompt the next command
  putStrLn ""
  putGameState gameState
  putStr $ "\n" ++ show currentPlayer ++ ": "
  hFlush stdout

  input <- getLine
  let maybeCommand = readMaybeCommand input :: Maybe Command

  case maybeCommand of
    Nothing -> do
      putStrLn $ "Invalid command: " ++ input
      play gameState
    Just command -> do
      let (globalPos, localPos) = commandToPos command
      case playGlobal globalPos localPos maybeNextPos currentPlayer globalBoard of
        Left e -> do
          putStrLn $ case e of
            OutOfBoard -> "You can't play outside the grid!"
            CaseOccupied -> "Somebody already played here!"
            ForcedToPlayLocal -> "You should play in local board according to the rules!"
            BoardAlreadyWon -> "Somebody already won this local board, you can't play here anymore!"
          play gameState
        Right newGlobalBoard ->
          if hasWinnerGlobal newGlobalBoard
          then do
            putGameState $ GS newGlobalBoard currentPlayer Nothing (Just command)
            putStrLn $ "\n" ++ show currentPlayer ++ " is the winner!\n"
          else if isATie newGlobalBoard
          then putStrLn "Nobody won, psss..."
          else play $ GS newGlobalBoard
                         (if currentPlayer == X then O else X)
                         (Just localPos)
                         (Just command)

playGame :: IO ()
playGame = do
  putStrLn "Beginning ASCII mode game."
  putStrLn "To play, enter a position like D3 for instance."
  putStrLn "If you don't know the rules yet, look here:"
  putStrLn "https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe"
  play beginningGameState

main :: IO ()
main = playGame
