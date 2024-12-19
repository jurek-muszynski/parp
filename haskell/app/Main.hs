module Main where

import Data.List (intercalate)
import qualified State
import qualified Gameplay

instructionsText :: String
instructionsText = intercalate "\n"
  [ "Available commands:"
  , "  n, s, e, w             -- Move in a direction."
  , "  a, b, c, d, ...        -- Interact with objects or people."
  , "  look                   -- Look around."
  , "  inventory              -- See what you are holding."
  , "  restart                -- Restart the game."
  , "  quit                   -- Quit the game."
  , "  help                   -- Show this list of commands."
  ]

-- Główna funkcja pętli gry
main :: IO ()
main = do
  putStrLn "Welcome to the game!"
  putStrLn instructionsText
  gameLoop State.initialState

-- Funkcja pętli gry
gameLoop :: State.State -> IO ()
gameLoop state = do
  putStrLn "\nWhat would you like to do?"
  putStr "> "
  command <- getLine
  case parseCommand command of
    Quit -> putStrLn "Thanks for playing!" >> return ()
    Restart -> putStrLn "Game restarted." >> gameLoop State.initialState
    Look -> do
      describeCurrentLocation state
      gameLoop state
    Inventory -> do
      showInventory state
      gameLoop state
    Move dir -> processMove dir state
    Interact option -> processInteraction option state
    Help -> do
      putStrLn instructionsText
      gameLoop state
    Unknown -> do
      putStrLn "I don't understand that command."
      gameLoop state

-- Funkcja obsługująca ruch gracza
processMove :: State.Direction -> State.State -> IO ()
processMove dir state =
  let State.Result msg newState = Gameplay.move dir state
  in do
    maybe (return ()) putStrLn msg
    gameLoop newState

-- Funkcja obsługująca interakcje
processInteraction :: Int -> State.State -> IO ()
processInteraction option state =
  let State.Result msg newState = Gameplay.interact option state
  in do
    maybe (return ()) putStrLn msg
    gameLoop newState

-- Wyświetla opis bieżącej lokacji
describeCurrentLocation :: State.State -> IO ()
describeCurrentLocation state = do
  let currentRoomIdx = State.playerLocation state
      (room, connections) = (State.worldMap state) !! currentRoomIdx
      directions = map (\(dir, (nextRoom, _)) -> (dir, (State.roomName . fst) (State.worldMap state !! nextRoom))) connections
      interactibles = Gameplay.interactionList state

  -- Room details
  putStrLn $ "\nYou are at the " ++ State.roomName room ++ "."
  maybe (return ()) putStrLn (State.roomDescription room)

  -- Available directions
  if null directions
    then putStrLn "There are no exits."
    else do
      putStrLn "Available directions:"
      mapM_ (\(dir, name) -> putStrLn $ "  " ++ show dir ++ " - " ++ name) directions

  -- Items and people in the room
  if null interactibles
    then putStrLn "You see no items or people to interact with."
    else do
      putStrLn "You see:"
      mapM_ (putStrLn . ("  - " ++) . showInteractible state) interactibles

-- Wyświetla przedmioty w ekwipunku
showInventory :: State.State -> IO ()
showInventory state = do
  if null (State.inventory state)
    then putStrLn "You are carrying: nothing"
    else do
      putStrLn "You are carrying:"
      mapM_ (putStrLn . (" - " ++) . show) (State.inventory state)

-- Wyświetla opis możliwych interakcji
showInteractible :: State.State -> Gameplay.Interactible -> String
showInteractible state (Gameplay.Item idx) =
  let item = (State.allItems state) !! idx
  in show item -- Use the Show instance of Item
showInteractible _ (Gameplay.Person idx) = show idx

-- Parsuje polecenie użytkownika
data Command = Move State.Direction
             | Interact Int
             | Look
             | Inventory
             | Restart
             | Quit
             | Help
             | Unknown

parseCommand :: String -> Command
parseCommand input = case input of
  "n" -> Move State.North
  "s" -> Move State.South
  "e" -> Move State.East
  "w" -> Move State.West
  "look" -> Look
  "inventory" -> Inventory
  "restart" -> Restart
  "help" -> Help -- Parse the help command
  "quit" -> Quit
  _ -> Unknown
