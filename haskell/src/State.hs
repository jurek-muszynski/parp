module State where

import Data.Tree (Tree)

-- Definicja początkowego stanu gry
initialState :: State
initialState = State
  { playerLocation = 0
  , inventory = []
  , inConversation = Nothing
  , allItems = []
  , worldMap =
    [ (0, [(South, (1, Nothing))])  -- Padded Cell to Hallway 1
    , (1, [(North, (0, Nothing)), (South, (2, Nothing))]) -- Hallway 1 paths
    , (2, [(North, (1, Nothing))])  -- Reception to Hallway 1
    ]
  }

-- Definicja struktury stanu gry
data State = State
  { playerLocation :: RoomIdx -- Aktualna lokalizacja gracza
  , inventory :: [ItemIdx] -- Lista przedmiotów w inwentarzu gracza
  , inConversation :: Maybe (PersonIdx, DialogOption) -- Aktualna rozmowa
  , allItems :: [Item] -- Lista wszystkich przedmiotów w grze
  , worldMap :: Map -- Mapa lokacji i połączeń
  } deriving (Show)

-- Identyfikatory dla lokacji, przedmiotów i osób
type RoomIdx = Int
type ItemIdx = Int
type PersonIdx = Int

-- Definicja przedmiotu
data Item = Item
  { name :: String
  } deriving (Show)

-- Typ mapy: lista par lokacji i możliwych połączeń
type Map = [(RoomIdx, [(Direction, (RoomIdx, RequiredItem))])]

-- Przedmiot wymagany do przejścia między lokacjami, `Nothing` oznacza brak wymagań
type RequiredItem = Maybe ItemIdx

-- Kierunki ruchu
data Direction = North | South | East | West deriving (Show, Eq)

-- Definicja lokacji
data Room = Room
  { roomName :: String
  , roomDescription :: Maybe String
  , itemsInARoom :: [ItemIdx]
  , peopleInARoom :: [Person]
  } deriving (Show)

-- Definicja osoby
data Person = Person
  { personName :: String
  , personDescription :: Maybe String
  , dialogTree :: [Tree (Int, String, State -> (String, State))]
  }

-- Ręczna instancja Show dla struktury Person
instance Show Person where
  show person =
    "Person { personName = " ++ show (personName person) ++
    ", personDescription = " ++ show (personDescription person) ++
    " }"

-- Opcje dialogowe
data DialogOption = Root | Other Int deriving (Show)

-- Wynik działania funkcji zmieniającej stan gry
data Result = Result
  { message :: Maybe String
  , newState :: State
  } deriving (Show)
