{-#LANGUAGE BlockArguments #-}
module Main where

import qualified Lib
import Data.Maybe
import System.Directory
import Control.Monad 
import Data.Function 
import System.Process 
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified CommandParser as CP 

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
interface Lib.Init b = do interface Lib.Command b

interface Lib.Command b = do 
    putStrLn "type Command below!(If you need help type 'help')"
    interactWithUser' b

interface Lib.Quit b = do 
    unless (null b) $ do
        putStrLn "saving current database"
        cur <- getCurrentDirectory 
        let filePath = cur ++ "/" ++ containerFileName
        writeFile filePath $ Lib.databaseToFileContent b
    putStrLn "Bye"
    return ()


interactWithUser' :: Lib.Database -> IO () 
interactWithUser' b = do 
    mc <- CP.parseCommand' <$> getLine
    case mc of 
        Right cmd -> do 
            (success, state, database) <- executeCommand cmd b
            if success then 
                interface state database
            else do
                putStrLn "ERROR: Command Failed"
                putStrLn "If you need help type 'help'"
                interface state b
        Left e -> do 
            putStr $ errorBundlePretty e
            putStrLn "If you need help type 'help'"
            interface Lib.Command b 


{- old version command parsing logic without megaparsec 

interactWithUser :: Lib.Database -> IO ()
interactWithUser b = do mc <- registerCommand
                        if isNothing mc  then
                          do putStrLn "Error: Invalid Command"
                             interactWithUser b
                        else
                          do (success, state, b) <- case mc of 
                                Nothing -> return (False, Lib.Command ,b)
                                (Just c) -> executeCommand c b
                             if success then
                                 interface state b
                             else
                                 do putStrLn "ERROR: Command Failed"
                                    putStrLn "If you need help type 'help'"
                                    interface state b

registerCommand :: IO (Maybe Lib.ValidCommand)
registerCommand = Lib.parseCommand <$> getLine
-}
executeCommand :: Lib.ValidCommand -> Lib.Database -> IO (Bool, Lib.UImode, Lib.Database)
executeCommand (Lib.Add name articlePath readmePath tags) b = do let articleData = Lib.mkData articlePath readmePath tags
                                                                     database =  Lib.addDatabase name articleData b
                                                                 return (True, Lib.Command, database)

executeCommand (Lib.Display operands) b = do 
    if null operands then 
        putStr $ Lib.displayDatabase b
    else do
        let name = head operands 
            ad = Lib.getFromDatabase name b
        putStrLn $ Lib.showArticleData ad 
    return (True, Lib.Command, b)

executeCommand Lib.Close b =  return (True, Lib.Quit, b)

executeCommand (Lib.Delete name) b = do
    let database = Lib.deleteDatabase name b
    return (True, Lib.Command, database)
    
executeCommand (Lib.RdDir options path) b = do 
    contents <- filter (\s -> s /= "." && s /= "..") <$> getDirectoryContents path
    putStr  "if you need some tags then put bellow(tags are added to each read articles)\n"
    tags <- words <$> getLine
    if Lib.OneDirectory  `elem` options then do
        let article = path ++ "/" ++ head contents
            readme = path ++ "/" ++ head (tail contents)
            ad = Lib.mkData article readme tags
        putStrLn "article name?"
        name <- getLine
        let b' = Lib.addDatabase name ad b
        return (True, Lib.Command, b')
    else do 

        let eggOfAds= 
                flip map contents \name ->
                let 
                    article = path ++ "/" ++ name ++ "/article.pdf"
                    readme = path ++ "/" ++ name ++ "/readme.md"
                in
                    (name, article, readme)
            database = flip fix (b, eggOfAds) \loop (b', eggs) ->
                if null eggs then 
                    b' 
                else 
                    let
                        (name, article, readme) = head eggs 
                        eggs' = tail eggs
                        ad = Lib.mkData article readme tags
                        b = Lib.addDatabase name ad b' 
                    in  loop (b, eggs')
        
        return (True, Lib.Command, database)

executeCommand (Lib.Edit name) b = do
    let ad = Lib.getFromDatabase name b
        readme = Lib.readmePath ad
    (_, _, _, ph) <- createProcess (proc "/usr/bin/nvim" [readme])
    exit <- waitForProcess ph
    putStrLn "edit finish!"
    return (True, Lib.Command, b)

initDatabase :: String -> IO Lib.Database
initDatabase file| null file = return Lib.emptyDatabase
                 | otherwise = return $ Lib.parseContainerFile file 

-- /mnt/c/users/tukud/desktop/intel_x8464/
-- /mnt/c/users/tukud/desktop/linux_abi/ 
-- /mnt/c/users/tukud/desktop/haskell2010/