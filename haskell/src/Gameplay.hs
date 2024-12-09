module Gameplay where

import qualified State

data Direction = North | South | West | East

move :: Direction -> State.State -> State.Result
move _ _ = error "TODO"

-- | Return the new state given the old state and the interaction number (1-indexed)
-- | from the interaction list (`interactionList`)
interact :: Int -> State.State -> State.Result
interact _ _ = error "TODO"


data Interactible = Item State.ItemIdx | Person State.PersonIdx

interactionList :: State.State -> [Interactible]
interactionList _ = error "TODO"
