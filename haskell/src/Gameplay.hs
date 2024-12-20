module Gameplay where

import Data.Tree (Tree(..))


import qualified State

move :: State.Direction -> State.State -> State.Result
move direction state =
  let currentRoomIdx = State.playerLocation state
      mapGraph = State.worldMap state
      paths = snd (mapGraph !! currentRoomIdx)
  in case lookup direction paths of
    Just (nextRoom, requiredItem) ->
      if maybe True (`elem` State.inventory state) requiredItem
      then State.Result Nothing state { State.playerLocation = nextRoom }
      else State.Result (Just "\ESC[32m\n[INFO]: You need a specific item to go that way. \ESC[0m") state
    Nothing -> State.Result (Just "\ESC[31m\n[ALERT]: You can't go that way! \ESC[0m") state

verifyCode :: State.State -> Int -> IO State.State
verifyCode state nextRoom = do
  putStrLn "\nYou need to enter a code to unlock the door:"
  putStr "> "
  code <- getLine
  if code == "01.01.1990" 
    then do
      putStrLn "\ESC[32m\n[INFO]: The door unlocks, and you move forward. \ESC[0m"
      return state { State.playerLocation = nextRoom }
    else do
      putStrLn "\ESC[31m\n[ALERT]: The code is incorrect. You cannot enter the room.\ESC[0m"
      return state


interact :: Int -> State.State -> State.Result
interact option state =
  let currentRoomIdx = State.playerLocation state
      (room, _) = (State.worldMap state) !! currentRoomIdx
      items = State.itemsInARoom room
      people = State.peopleInARoom room
      interactibles = zip [0..] (map Left items ++ map Right people)
  in case lookup option interactibles of
    Just (Left itemIdx) ->
      if itemIdx `elem` State.inventory state
      then State.Result (Just "\ESC[31m\n[ALERT]: You already have this item. \ESC[0m") state
      else
        let newState = state { State.inventory = itemIdx : State.inventory state }
        in State.Result (Just "\ESC[32m\n[INFO]: You picked up an item. \ESC[0m") newState
    Just (Right person) ->
      let personName = State.personName person
      in State.Result (Just $ "You talk to " ++ personName) (state { State.inConversation = Just (person, 0) })
    Nothing -> State.Result (Just "\ESC[31m\n[ALERT]: Invalid interaction option. \ESC[0m") state

data Interactible = Item State.ItemIdx | Person State.Person deriving (Show)

interactionList :: State.State -> [Interactible]
interactionList state =
  let currentRoomIdx = State.playerLocation state
      (room, _) = (State.worldMap state) !! currentRoomIdx
      items = map Item $ State.itemsInARoom room
      people = map Person $ State.peopleInARoom room
  in items ++ people

takeItem :: State.State -> String -> IO State.State
takeItem state itemName =
  let currentRoomIdx = State.playerLocation state
      (room, _) = (State.worldMap state) !! currentRoomIdx
      itemsInRoom = State.itemsInARoom room
      allItems = State.allItems state
      maybeItemIdx = lookup itemName [(State.name (allItems !! idx), idx) | idx <- itemsInRoom]
  in case maybeItemIdx of
       Just itemIdx ->
         let updatedRoom = room { State.itemsInARoom = filter (/= itemIdx) itemsInRoom }
             updatedMap = take currentRoomIdx (State.worldMap state)
                          ++ [(updatedRoom, snd (State.worldMap state !! currentRoomIdx))]
                          ++ drop (currentRoomIdx + 1) (State.worldMap state)
             updatedState = state { State.inventory = itemIdx : State.inventory state, State.worldMap = updatedMap }
         in do
              putStrLn $ "\ESC[32m\n[ALERT]: You picked up the " ++ itemName ++ ".\ESC[0m"
              return updatedState
       Nothing -> do
         putStrLn $ "\ESC[31m\n[ALERT]: There is no " ++ itemName ++ " here.\ESC[0m"
         return state

runDialog :: State.State -> State.Person -> IO State.State
runDialog state person = do
  let dialog = State.dialogTree person
  processDialogTree state dialog
  where
    processDialogTree state (Node (optionId, text, action) subOptions) = do
      putStrLn text
      if null subOptions
        then do
          let (msg, newState) = action state
          putStrLn msg
          return newState
        else do
          mapM_ (\(idx, Node (_, optText, _) _) -> putStrLn $ show idx ++ ". " ++ optText) (zip [0..] subOptions)
          putStrLn "\nChoose an option (number):"
          choice <- getLine
          case reads choice of
            [(idx, "")] | idx >= 0 && idx < length subOptions -> do
              let Node (_, _, nextAction) nextSubOptions = subOptions !! idx
                  (msg, newState) = nextAction state
              putStrLn msg
              if null nextSubOptions
                then return newState
                else processDialogTree newState (Node (optionId, text, nextAction) nextSubOptions)
            _ -> do
              putStrLn "\ESC[31m\n[ALERT]: Invalid choice. \ESC[0m"
              processDialogTree state (Node (optionId, text, action) subOptions)
