module Gameplay where

import qualified State

-- Ruch gracza między lokacjami
move :: State.Direction -> State.State -> State.Result
move direction state =
  let currentRoomIdx = State.playerLocation state
      mapGraph = State.worldMap state
  in case lookup currentRoomIdx mapGraph of
       Just paths -> case lookup direction paths of
         Just (nextRoom, requiredItem) ->
           if maybe True (`elem` State.inventory state) requiredItem
           then let newState = state { State.playerLocation = nextRoom }
                in State.Result (Just $ "You moved " ++ show direction ++ ".") newState
           else State.Result (Just "You need a specific item to go that way.") state
         Nothing -> State.Result (Just "You can't go that way.") state
       Nothing -> State.Result (Just "Invalid location.") state

-- Interakcje z przedmiotami i osobami
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
         then State.Result (Just "You already have this item.") state
         else
           let newState = state { State.inventory = itemIdx : State.inventory state }
           in State.Result (Just "You picked up an item.") newState
       Just (Right person) ->
         let personName = State.personName person
         in State.Result (Just $ "You talked to " ++ personName) state
       Nothing -> State.Result (Just "Invalid interaction option.") state

-- Lista możliwych interakcji w danej lokacji
data Interactible = Item State.ItemIdx | Person State.Person deriving (Show)

interactionList :: State.State -> [Interactible]
interactionList state =
  let currentRoomIdx = State.playerLocation state
      (room, _) = (State.worldMap state) !! currentRoomIdx
      items = map Item $ State.itemsInARoom room
      people = map Person $ State.peopleInARoom room
  in items ++ people
