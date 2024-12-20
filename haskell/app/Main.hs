module Main where

import System.IO (hFlush, stdout)

import Data.List (intercalate)
import qualified State
import qualified Gameplay

instructionsText :: String
instructionsText = intercalate "\n"
  [ "\nAvailable commands:"
  , "  n, s, e, w             -- Move in a direction."
  , "  a, b, c, d, ...        -- Interact with objects or people."
  , "  look                   -- Look around."
  , "  inventory              -- See what you are holding."
  , "  restart                -- Restart the game."
  , "  quit                   -- Quit the game."
  , "  help                   -- Show this list of commands."
  , "  take [item]            -- Take an item."
  , "  talk                   -- Talk to a person."
  ]

main :: IO ()
main = do
  putStrLn ("\ESC[1m" ++ "\nWelcome to the game!" ++ "\ESC[0m")
  putStrLn instructionsText
  gameLoop State.initialState

gameLoop :: State.State -> IO ()
gameLoop state = do
  putStrLn "\nWhat would you like to do?"
  putStr "> "
  hFlush stdout
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
    Move dir -> do
      processMove dir state
      describeCurrentLocation state
    Interact option -> processInteraction option state
    Take itemName -> do
      newState <- Gameplay.takeItem state itemName
      gameLoop newState
    Dialog -> do
      newState <- processDialog state
      gameLoop newState
    Help -> do
      putStrLn instructionsText
      gameLoop state
    Unknown -> do
      putStrLn "\ESC[31m\n[ALERT]: Wrong command, try again! \ESC[0m"
      gameLoop state


processDialog :: State.State -> IO State.State
processDialog state =
  let currentRoomIdx = State.playerLocation state
      (room, _) = (State.worldMap state) !! currentRoomIdx
      peopleInRoom = State.peopleInARoom room
  in if null peopleInRoom
       then do
         putStrLn "\ESC[31m\n[ALERT]: There is no one to talk here. \ESC[0m"
         return state
       else do
         mapM_ (\(idx, person) -> putStrLn $ show idx ++ ". " ++ State.personName person) (zip [0..] peopleInRoom)
         putStrLn "\nChoose a person to talk to (number):"
         choice <- getLine
         case reads choice of
           [(idx, "")] | idx >= 0 && idx < length peopleInRoom -> 
             Gameplay.runDialog state (peopleInRoom !! idx)
           _ -> do
             putStrLn "\ESC[31m\n[ALERT]: Invalid choice. \ESC[0m"
             return state

processMove :: State.Direction -> State.State -> IO ()
processMove dir state = do
  let result = Gameplay.move dir state
  case result of
    State.Result Nothing newState -> do
      if State.playerLocation state == 4 && dir == State.West
        then do
          updatedState <- Gameplay.verifyCode state 7
          gameLoop updatedState
      else if State.playerLocation state == 9
        then return ()
        else gameLoop newState
    State.Result (Just msg) newState -> do
      putStrLn msg
      gameLoop newState


verifyCodeIO :: State.State -> Int -> IO State.State
verifyCodeIO state nextRoom = do
  putStrLn "\nPlease enter the code to unlock the door:"
  putStr "> "
  code <- getLine
  if code == "01.01.1990"
    then do
      putStrLn "The door unlocks, and you proceed to the next room."
      return state { State.playerLocation = nextRoom }
    else do
      putStrLn "Incorrect code. You cannot enter the room."
      return state

processInteraction :: Int -> State.State -> IO ()
processInteraction option state =
  let State.Result msg newState = Gameplay.interact option state
  in do
    maybe (return ()) putStrLn msg
    gameLoop newState

describeCurrentLocation :: State.State -> IO ()
describeCurrentLocation state = do
  let currentRoomIdx = State.playerLocation state
      (room, connections) = (State.worldMap state) !! currentRoomIdx
      directions = map (\(dir, (nextRoom, _)) -> (dir, (State.roomName . fst) (State.worldMap state !! nextRoom))) connections
      interactibles = Gameplay.interactionList state

  putStrLn $ "\ESC[1m" ++ "\nYou are at the " ++ State.roomName room ++ ".\n" ++ "\ESC[0m"
  maybe (return ()) putStrLn (State.roomDescription room)

  if null directions && null interactibles
    then putStrLn "Thanks for playing!" >> return () -- nie działające wyjście z gry
    else do
      if null directions
        then putStrLn "\nThere are no exits."
        else do
          putStrLn "\nAvailable directions:"
          mapM_ (\(dir, name) -> putStrLn $ "  " ++ show dir ++ " - " ++ name) directions

      if null interactibles
        then putStrLn "\nYou see no items or people to interact with."
        else do
          putStrLn "\nYou see:"
          mapM_ (putStrLn . ("  - " ++) . showInteractible state) interactibles

showInventory :: State.State -> IO ()
showInventory state = do
  if null (State.inventory state)
    then putStrLn "\nYou are carrying: nothing"
    else do
      putStrLn "\nYou are carrying:"
      let items = map (\idx -> (State.allItems state) !! idx) (State.inventory state)
      mapM_ (putStrLn . (" - " ++) . State.name) items


showInteractible :: State.State -> Gameplay.Interactible -> String
showInteractible state (Gameplay.Item idx) =
  let item = (State.allItems state) !! idx
  in show item
showInteractible _ (Gameplay.Person idx) = show idx

data Command
  = Move State.Direction
  | Interact Int
  | Look
  | Inventory
  | Restart
  | Quit
  | Help
  | Take String
  | Dialog
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
  "help" -> Help
  "quit" -> Quit
  "talk" -> Dialog
  ('t':'a':'k':'e':' ':rest) -> Take rest
  _ -> Unknown
