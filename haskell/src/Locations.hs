module Locations where

import State

data Direction = North | South | East | West deriving (Show, Eq)

data Room = Room
  { roomName :: String
  , roomDescription :: String
  , roomItems :: [ItemIdx]
  , roomPeople :: [PersonIdx]
  } deriving (Show)

type Map = [(RoomIdx, [(Direction, (RoomIdx, RequiredItem))])]

mapGraph :: Map
mapGraph =
  [ (0, [(South, (1, Nothing))])
  , (1, [(North, (0, Nothing)), (South, (2, Nothing))])
  , (2, [(North, (1, Nothing))])
  ]

locations :: [Room]
locations =
  [ Room "Padded Cell" "A small padded cell with no windows." [] []
  , Room "Hallway 1" "A dimly lit hallway." [] []
  , Room "Reception" "A reception area with a desk and a receptionist." [] [0]
  ]

