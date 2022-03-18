module Main where

import Lib
import Data.Maybe

main = interfaceInit

interfaceInit :: IO ()
interfaceInit = do putStrLn "Hello!"
                   b <- initDatabase
                   interface Init b

interface :: UImode -> Database -> IO ()
interface Init b = do putStrLn "If you need help type 'help'"
                      interface Command b
interface Command b = interactWithUser b
interface Quit b = do putStrLn "Bye"
                      return ()

interactWithUser :: Database -> IO ()

interactWithUser b = do mc <- registerCommand
                        if isNothing mc  then
                          do putStrLn "Error: Invalid Command"
                             interactWithUser b
                        else
                          do (success, state, b) <- doCommand mc b
                             if success then
                                 interface state b
                             else
                                 do putStrLn "ERROR: Command Failed"
                                    putStrLn "If you need help type 'help'"
                                    interface state b

registerCommand :: IO (Maybe ValidCommand)
registerCommand = parseCommand <$> getLine

doCommand :: Maybe ValidCommand -> Database -> IO (Bool, UImode, Database)
doCommand Nothing b = return (False, Command ,b)
doCommand (Just x) b = executeCommand x b

executeCommand :: ValidCommand -> Database -> IO (Bool, UImode,Database)
executeCommand (Add name articlePath readmePath tags) b = do let articleData = mkData articlePath readmePath tags
                                                             database <- addDatabase name articleData b
                                                             return (True, Command, database)
executeCommand Display b = do putStr $ displayDatabase b
                              return (True, Command, b)
executeCommand Close b =  return (True, Quit, b)
executeCommand (Delete name) b = return (False, Command, b) -- TODO
