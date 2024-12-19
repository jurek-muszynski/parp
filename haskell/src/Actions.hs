module Actions where

import State
import Locations
import Items

move :: Direction -> State -> Result
move dir state =
  case lookup (playerLocation state) mapGraph of
    Just paths -> case lookup dir paths of
      Just newLoc -> Result (Just "You move.") (state { playerLocation = newLoc })
      Nothing     -> Result (Just "You can't go that way.") state
    Nothing -> Result (Just "Unknown location.") state

takeItem :: ItemIdx -> State -> Result
takeItem itemIdx state
  | itemIdx `elem` inventory state = Result (Just "You're already holding this item.") state
  | otherwise = Result (Just "You picked up an item.") state { inventory = itemIdx : inventory state }
