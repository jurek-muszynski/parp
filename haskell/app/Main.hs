module Main where

import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, maybe)
import Text.Read (readMaybe)

import State (State, Result(..), initialState, message, newState)
import Gameplay

instructionsText :: String
instructionsText = intercalate "\n"
   [ "Available commands:"
   , "  restart                -- to restart the game."
   , "  n  s  e  w             -- to go in that direction."
   , "  a  b  c  d  ...        -- to select an interaction"
   , "  drop(Object)           -- to put down an object."
   , "  inspect(Object)        -- to inspect an object."
   , "  look                   -- to look around you again."
   , "  inventory              -- to see what you are holding."
   , "  instructions           -- to see this message again."
   , "  halt                   -- to end the game and quit."
   , "  enter_code(Direction)  -- to enter the code for a door (surround it with parenthesis)." ]

readCommand :: IO String
readCommand = do
  putStr "> "
  xs <- getLine
  return xs

gameLoop :: State -> IO ()
gameLoop state = do
  cmd <- readCommand
  case cmd of
    "instructions" -> do putStrLn instructionsText
                         gameLoop state
    "restart" -> gameLoop state
    "quit" -> return ()
    _ -> case result of
           Just (Result{message, newState}) -> do case message of
                                                    Just msg -> putStrLn msg
                                                    Nothing -> return ()
                                                  gameLoop newState
           Nothing -> do putStrLn $ "Unknown command: '" ++ cmd ++ "'\n"
                         gameLoop state
           where result | isJust direction = Just $ Gameplay.move (fromJust direction) state
                        | isJust number    = Just $ Gameplay.interact (fromJust number) state
                        | otherwise        = Nothing
                        where number = readMaybe cmd
                              direction = case cmd of
                                            "n" -> Just North
                                            "s" -> Just South
                                            "w" -> Just West
                                            "e" -> Just East
                                            _ -> Nothing

main :: IO ()
main = do
  putStrLn instructionsText
  gameLoop initialState
