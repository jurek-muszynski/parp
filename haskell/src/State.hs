module State where

import Data.Tree (Tree(..))

-- Definicja początkowego stanu gry
initialState :: State
initialState = State
  { playerLocation = 0
  , inventory = []
  , inConversation = Nothing
  , allItems =
    [ Item
        { name = "Padded Cell Key"
        , description = "A small, slightly rusted key that opens the door to the padded cell. It feels cold to the touch and has a faint engraving on it."
        }
    , Item
        { name = "Exit Key"
        , description = "A sturdy key that opens the exit door. It has a unique shape and seems to be well-used, indicating frequent use."
        }
    , Item
        { name = "Patient File"
        , description = unlines
            [ "A file containing detailed information about a patient. The pages are worn and some notes are scribbled in the margins, hinting at a troubled history."
            , "Patient File - Patient 12:"
            , "Name: Unknown"
            , "Date of Birth: 01.01.1990"
            ]
        }
    , Item
        { name = "Discharge Form"
        , description = unlines
            [ "A form that seems to be a discharge form. It is filled out with a patient's information and seems to be ready for processing."
            , "Discharge Form - Patient 12:"
            , "Name: Unknown"
            , "Date of discharge: 31.02.2020"
            ]
        }
    , Item
        { name = "Set of Notes"
        , description = unlines
            [ "A set of handwritten notes, filled with observations and theories. The handwriting is hurried and some parts are difficult to read, but they seem to contain important information."
            , "Report - 1/2/2020: Patient 12 is showing signs of improvement. The new treatment seems to be working."
            , "Report - 1/3/2020: Patient 12 is showing signs of distress. The new treatment is causing unexpected side effects."
            , "Report - 1/4/2020: Patient 12 is unresponsive. The new treatment has failed. The patient is to be moved to the basement for further observation."
            , "Report - 1/5/2020: Patient 12 has been moved to the basement. The code is his date of birth. The patient is to be monitored closely."
            ]
        }
    ]

  , worldMap =
    [ (Room
        { roomName = "Padded Cell"
        , roomDescription = Just "The walls are covered in a soft, padded material. There are no windows, the only connection with the outside world is a heavy door."
        , itemsInARoom = [0] -- Padded Cell Key
        , peopleInARoom = [] }
      , [(South, (1, Nothing))])  -- Padded Cell to Hallway 1

    , (Room
        { roomName = "Hallway number 1"
        , roomDescription = Just "The walls here are lined with faded paintings of serene landscapes and old photographs of what appears to be hospital staff."
        , itemsInARoom = []
        , peopleInARoom = [] }
      , [(North, (0, Nothing)), (South, (2, Nothing))]) -- Hallway 1 paths

    , (Room
        { roomName = "Reception"
        , roomDescription = Just "It is mostly empty, apart from a few flower pots set against the walls and a desk in the middle. Behind it sits a middle-aged woman illuminated by the computer screen."
        , itemsInARoom = []
        , peopleInARoom = [receptionist] } -- Store receptionist directly
      , [(North, (1, Nothing)), (East, (3, Nothing)), (South, (4, Nothing)), (West, (8, Nothing))]) -- Reception to Hallway 1

    , (Room
        { roomName = "Restroom"
        , roomDescription = Just "The restroom is small and sterile, with cracked mirrors above the sinks and water dripping from a leaky faucet."
        , itemsInARoom = [3] -- Discharge Form
        , peopleInARoom = [] }
      , [(West, (2, Nothing))]) -- Restroom to Reception


    , (Room
        { roomName = "Hallway number 2"
        , roomDescription = Just "The hallway is dimly lit, with flickering lights casting long shadows on the walls. The air is heavy with the smell of disinfectant."
        , itemsInARoom = []
        , peopleInARoom = [] }
      , [(North, (2, Nothing)), (East, (5, Nothing)), (South, (6, Nothing)), (West, (7, Nothing))]) -- Hallway 2 paths

    , (Room
        { roomName = "Exam Room 1"
        , roomDescription = Just "This room is stark and clinical, with a single examination table at its center covered in worn white sheets."
        , itemsInARoom = [2] -- Patient File
        , peopleInARoom = [doctor] } -- Store doctor directly
      , [(West, (4, Nothing))]) -- Exam Room 1 to Hallway 2

    , (Room
        { roomName = "Exam Room 2"
        , roomDescription = Just "This room is eerily silent. The examination table is overturned, and scattered papers litter the floor."
        , itemsInARoom = [4] -- Set of Notes
        , peopleInARoom = [] }
      , [(North, (4, Nothing))]) -- Exam Room 2 to Hallway 2

    , (Room
        { roomName = "Basement"
        , roomDescription = Just "The air here is cold and damp, with the faint hum of machinery echoing throughout the space."
        , itemsInARoom = [1] -- Exit Key
        , peopleInARoom = [] }
      , [(East, (4, Nothing))]) -- Basement to Hallway 2

    , (Room
        { roomName = "Exit"
        , roomDescription = Just "The door leads to the outside world, with sunlight streaming in and the sound of birds chirping in the distance."
        , itemsInARoom = [] -- Exit Key
        , peopleInARoom = [] }
      , [(East, (2, Nothing))])
    ]
  }

-- Helper Functions
askReceptionistBus :: State -> (String, State)
askReceptionistBus state =
  ("The receptionist tells you the bus departs at 5:00 PM.", state)

askDoctorDischarge :: State -> (String, State)
askDoctorDischarge state =
  ("The doctor approves your discharge.", state)

-- Define the receptionist
receptionist :: Person
receptionist = Person
  { personName = "Receptionist"
  , personDescription = "A middle-aged woman sits behind the desk, typing on a computer."
  , dialogTree = [Node (1, "Ask about the bus schedule.", askReceptionistBus) []]
  }

-- Define the doctor
doctor :: Person
doctor = Person
  { personName = "Doctor"
  , personDescription = "The doctor looks up from his desk, appearing calm but focused."
  , dialogTree = [Node (1, "Ask for discharge.", askDoctorDischarge) []]
  }


-- Definicja struktury stanu gry
data State = State
  { playerLocation :: RoomIdx -- Aktualna lokalizacja gracza
  , inventory :: [ItemIdx] -- Lista przedmiotów w inwentarzu gracza
  , inConversation :: Maybe (Person, DialogOption) -- Aktualna rozmowa
  , allItems :: [Item] -- Lista wszystkich przedmiotów w grze
  , worldMap :: Map -- Mapa lokacji i połączeń
  } deriving (Show)

-- Identyfikatory dla lokacji, przedmiotów i osób
type RoomIdx = Int
type ItemIdx = Int

-- Definicja przedmiotu
data Item = Item
  { name :: String
  , description :: String
  }

-- Typ mapy: lista par lokacji i połączeń
type Map = [(Room, [(Direction, (RoomIdx, RequiredItem))])]

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
  , personDescription :: String
  , dialogTree :: [Tree (Int, String, State -> (String, State))]
  }

instance Show Item where
  show item = show (name item) ++ " - " ++ show (description item)

instance Show Person where
  show person =
    show (personName person) ++ " - " ++ show (personDescription person)

-- Opcje dialogowe
data DialogOption = Root | Other Int deriving (Show)

-- Wynik działania funkcji zmieniającej stan gry
data Result = Result
  { message :: Maybe String
  , newState :: State
  } deriving (Show)
