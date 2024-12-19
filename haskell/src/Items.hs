module Items where

import State

data Item = Item
  { itemName :: String
  , itemDescription :: String
  } deriving (Show)

items :: [Item]
items =
  [ Item "Padded Cell Key" "A rusted key that opens the padded cell door."
  , Item "Exit Key" "A sturdy key that opens the exit door."
  , Item "Patient File" "A file containing patient information."
  , Item "Discharge Form" "A form required for discharge."
  , Item "Set of Notes" "A set of handwritten notes."
  ]
