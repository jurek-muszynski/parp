module Main where

import Data.List (intercalate)
import qualified State
import qualified Gameplay
import qualified Locations

instructionsText :: String
instructionsText = intercalate "\n"
  [ "Available commands:"
  , "  n, s, e, w             -- Move in a direction."
  , "  a, b, c, d, ...        -- Interact with objects or people."
  , "  look                   -- Look around."
  , "  inventory              -- See what you are holding."
  , "  restart                -- Restart the game."
  , "  quit                   -- Quit the game."
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
    Unknown -> do
      putStrLn "I don't understand that command."
      gameLoop state

-- Funkcja obsługująca ruch gracza
processMove :: Gameplay.Direction -> State.State -> IO ()
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
      (room, _) = (State.worldMap state) !! currentRoomIdx
  putStrLn $ "\n" ++ State.roomName room
  maybe (return ()) putStrLn (State.roomDescription room)
  putStrLn "You see:"
  mapM_ (putStrLn . (" - " ++) . showInteractible) (Gameplay.interactionList state)

-- Wyświetla przedmioty w ekwipunku
showInventory :: State.State -> IO ()
showInventory state = do
  putStrLn "You are carrying:"
  mapM_ putStrLn (map (\itemIdx -> "- " ++ State.name ((State.allItems state) !! itemIdx)) (State.inventory state))

-- Wyświetla opis możliwych interakcji
showInteractible :: Gameplay.Interactible -> String
showInteractible (Gameplay.Item idx) = "Item #" ++ show idx
showInteractible (Gameplay.Person idx) = "Person #" ++ show idx

-- Parsuje polecenie użytkownika
data Command = Move Gameplay.Direction
             | Interact Int
             | Look
             | Inventory
             | Restart
             | Quit
             | Unknown

parseCommand :: String -> Command
parseCommand input = case input of
  "n" -> Move Gameplay.North
  "s" -> Move Gameplay.South
  "e" -> Move Gameplay.East
  "w" -> Move Gameplay.West
  "look" -> Look
  "inventory" -> Inventory
  "restart" -> Restart
