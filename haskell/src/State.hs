module State where

import Data.Tree (Tree)

initialState :: State
initialState = State
  { playerLocation = 0
  , inventory = []
  , inConversation = Nothing
  , allItems = []
  , worldMap =
    [(State.Room
      { roomName = "padded cell"
      , roomDescription = Nothing
      , itemsInARoom = []
      , peopleInARoom = [] }, [])] }

data State = State
  { playerLocation :: RoomIdx
  , inventory :: [ItemIdx]
  , inConversation :: Maybe (PersonIdx, DialogOption)
  , allItems :: [Item]
  , worldMap :: Map }

type RoomIdx = Int
type ItemIdx = Int
type PersonIdx = Int
data Item = Item { name :: String }

-- | Map of all the game locations in a shape of a graph
type Map = [(Room, [(RoomIdx, RequiredItem)])]
-- | Item required to go to another room, `Nothing` if one isn't needed
type RequiredItem = Maybe ItemIdx

data Room = Room
  { roomName :: String
  , roomDescription :: Maybe String
  , itemsInARoom :: [ItemIdx]
  , peopleInARoom :: [Person] }

data Person = Person
  { personName :: String
  , personDescription :: Maybe String
  -- Tree with an empty root, which is the same as a list of subtrees
  -- Each node has a tuple of a dialog option id, a question
  -- and a function returning a response to the question and the new state
  , dialogTree :: [Tree (Int, String, State -> (String, State))] }

data DialogOption = Root | Other Int

data Result = Result
  { message :: Maybe String
  , newState :: State }
