module Main where

import qualified Lib
import Data.Maybe
import System.Directory
import Control.Monad 

containerFileName :: String 
containerFileName = "data.txt"

containerFilePath :: String -> String 
containerFilePath dir = dir ++ "/" ++ containerFileName

main = interfaceInit

interfaceInit :: IO ()
interfaceInit = do putStrLn "Hello!"
                   cur <- getCurrentDirectory 
                   success <- doesFileExist $ containerFilePath cur
                   if success then
                    do file <- readFile $ containerFilePath cur 
                       b <- initDatabase file
                       interface Lib.Init b  
                   else 
                       interface Lib.Init Lib.emptyDatabase

interface :: Lib.UImode -> Lib.Database -> IO ()
interface Lib.Init b = do putStrLn "If you need help type 'help'"
                          interface Lib.Command b
interface Lib.Command b = interactWithUser b
interface Lib.Quit b = do 
    unless (null b) $ do
        putStrLn "saving current database"
        cur <- getCurrentDirectory 
        let filePath = cur ++ "/" ++ containerFileName
        writeFile filePath $ Lib.databaseToFileContent b
    putStrLn "Bye"
    return ()

interactWithUser :: Lib.Database -> IO ()

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

registerCommand :: IO (Maybe Lib.ValidCommand)
registerCommand = Lib.parseCommand <$> getLine

doCommand :: Maybe Lib.ValidCommand -> Lib.Database -> IO (Bool, Lib.UImode, Lib.Database)
doCommand Nothing b = return (False, Lib.Command ,b)
doCommand (Just x) b = executeCommand x b

executeCommand :: Lib.ValidCommand -> Lib.Database -> IO (Bool, Lib.UImode, Lib.Database)
executeCommand (Lib.Add name articlePath readmePath tags) b = do let articleData = Lib.mkData articlePath readmePath tags
                                                                     database =  Lib.addDatabase name articleData b
                                                                 return (True, Lib.Command, database)
executeCommand Lib.Display b = do putStr $ Lib.displayDatabase b
                                  return (True, Lib.Command, b)
executeCommand Lib.Close b =  return (True, Lib.Quit, b)
executeCommand (Lib.Delete name) b = do
    let database = Lib.deleteDatabase name b
    return (True, Lib.Command, database)


initDatabase :: String -> IO Lib.Database
initDatabase file| null file = return Lib.emptyDatabase
                 | otherwise = return $ Lib.parseContainerFile file 