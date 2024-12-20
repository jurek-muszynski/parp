module State where

import Data.Tree (Tree(..))

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
        , itemsInARoom = [0] 
        , peopleInARoom = [] }
      , [(South, (1, Just 0))])

    , (Room
        { roomName = "Hallway number 1"
        , roomDescription = Just "The walls here are lined with faded paintings of serene landscapes and old photographs of what appears to be hospital staff."
        , itemsInARoom = []
        , peopleInARoom = [] }
      , [(North, (0, Nothing)), (South, (2, Nothing))])

    , (Room
        { roomName = "Reception"
        , roomDescription = Just "It is mostly empty, apart from a few flower pots set against the walls and a desk in the middle. Behind it sits a middle-aged woman illuminated by the computer screen."
        , itemsInARoom = []
        , peopleInARoom = [receptionist] } 
      , [(North, (1, Nothing)), (East, (3, Nothing)), (South, (4, Nothing)), (West, (8, Just 1))]) 

    , (Room
        { roomName = "Restroom"
        , roomDescription = Just "The restroom is small and sterile, with cracked mirrors above the sinks and water dripping from a leaky faucet."
        , itemsInARoom = [3] 
        , peopleInARoom = [] }
      , [(West, (2, Nothing))])


    , (Room
        { roomName = "Hallway number 2"
        , roomDescription = Just "The hallway is dimly lit, with flickering lights casting long shadows on the walls. The air is heavy with the smell of disinfectant."
        , itemsInARoom = []
        , peopleInARoom = [] }
      , [(North, (2, Nothing)), (East, (5, Nothing)), (South, (6, Nothing)), (West, (7, Nothing))])

    , (Room
        { roomName = "Exam Room 1"
        , roomDescription = Just "This room is stark and clinical, with a single examination table at its center covered in worn white sheets."
        , itemsInARoom = [2]
        , peopleInARoom = [doctor] }
      , [(West, (4, Nothing))])

    , (Room
        { roomName = "Exam Room 2"
        , roomDescription = Just "This room is eerily silent. The examination table is overturned, and scattered papers litter the floor."
        , itemsInARoom = [4]
        , peopleInARoom = [] }
      , [(North, (4, Nothing))])

    , (Room
        { roomName = "Basement"
        , roomDescription = Just "The air here is cold and damp, with the faint hum of machinery echoing throughout the space."
        , itemsInARoom = [1]
        , peopleInARoom = [] }
      , [(East, (4, Nothing))])

    , (Room
        { roomName = "Exit"
        , roomDescription = Just "The door leads to the outside world, with sunlight streaming in and the sound of birds chirping in the distance."
        , itemsInARoom = []
        , peopleInARoom = [] }
      , [(East, (2, Nothing)), (West, (9, Just 1))])

    , (Room
        { roomName = "Bus Stop"
        , roomDescription = Just $ unlines
        [ "You step out of the asylum into the crisp evening air."
        , "The world feels strange, almost unfamiliar."
        , "You walk to the nearest bus stop and sit down."
        , "A bus arrives. The sign reads: 'Bialystok'."
        , "You board the bus, unsure of what lies ahead, but hopeful."
        , "The engine hums as the bus drives away, taking you toward a new beginning..."
        , "*** THE END ***"
        ]
        , itemsInARoom = []
        , peopleInARoom = [] }
      , [])  
    ]
  }
askReceptionistBus :: State -> (String, State)
askReceptionistBus state =
  ("\ESC[32m\n[INFO]: The receptionist tells you the bus departs at 5:00 PM. \ESC[0m", state)

askDoctorDischarge :: State -> (String, State)
askDoctorDischarge state =
  ("\ESC[32m\n[INFO]: The doctor approves your discharge. \ESC[0m", state)

receptionist :: Person
receptionist = Person
  { personName = "Receptionist"
  , personDescription = "A middle-aged woman sits behind the desk, typing on a computer."
  , dialogTree = Node 
      (0, "\nYou see a receptionist. What would you like to ask?", \state -> ("", state))
      [ Node (1, "Ask about the bus schedule.", askReceptionistBus) []
      ]
  }

doctor :: Person
doctor = Person
  { personName = "Doctor"
  , personDescription = "The doctor looks up from his desk, appearing calm but focused."
  , dialogTree = Node 
      (0, "The doctor looks at you attentively. What would you like to ask?", \state -> ("", state))
      [ Node (1, "Ask for discharge.", askDoctorDischarge) []
      ]
  }



data State = State
  { playerLocation :: RoomIdx 
  , inventory :: [ItemIdx] 
  , inConversation :: Maybe (Person, DialogOption) 
  , allItems :: [Item] 
  , worldMap :: Map
  } deriving (Show)

type RoomIdx = Int
type ItemIdx = Int

data Item = Item
  { name :: String
  , description :: String
  }

type Map = [(Room, [(Direction, (RoomIdx, RequiredItem))])]

type RequiredItem = Maybe ItemIdx

data Direction = North | South | East | West deriving (Show, Eq)

data Room = Room
  { roomName :: String
  , roomDescription :: Maybe String
  , itemsInARoom :: [ItemIdx]
  , peopleInARoom :: [Person]
  } deriving (Show)

data Person = Person
  { personName :: String
  , personDescription :: String
  , dialogTree :: Tree (Int, String, State -> (String, State))
  }

instance Show Item where
  show item = show (name item) ++ " - " ++ show (description item)

instance Show Person where
  show person =
    show (personName person) ++ " - " ++ show (personDescription person)

data DialogOption = Root | Other Int deriving (Show)

data Result = Result
  { message :: Maybe String
  , newState :: State
  } deriving (Show)
