{-#LANGUAGE BlockArguments #-}
{-#LANGUAGE OverloadedStrings #-}
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
import qualified SQLHandler as DB
import qualified System.IO.Streams as ST
import qualified Data.Text as T
import qualified Myget as MG  


containerFileName :: String 
containerFileName = "data.txt"

containerFilePath :: String -> String 
containerFilePath dir = dir ++ "/" ++ containerFileName

main = interfaceInit'

interfaceInit' :: IO () 
interfaceInit' = do 
    putStrLn "Hello!"
    conn <- DB.connectDB DB.myConnectInfo
    putStrLn "Connect to your database!"
    interface' Lib.Command conn


interface' :: Lib.UImode -> DB.DB -> IO () 
interface' Lib.Init _ = return () -- DO NOT USE 
interface' Lib.Command  db = do 
    putStrLn "type Command below!(If you need help type 'help')"
    command <- CP.parseCommand <$> MG.getLine
    case command of
        Right cmd -> do
            (success, state) <- executeCommand' cmd db
            if success then 
                interface' state db
            else do
                putStrLn "ERROR: Command Failed"
                putStrLn "If you need help type 'help'"
                interface' state db
        Left e -> do 
            putStr $ errorBundlePretty e
            putStrLn "If you need help type 'help'"
            interface' Lib.Command db
interface' Lib.Quit db = do 
    putStrLn "Closing your database"
    DB.closeDB db
    putStr "Bye"
    return () 


executeCommand' :: Lib.ValidCommand -> DB.DB -> IO (Bool, Lib.UImode )
executeCommand' cmd db = case cmd of 
    Lib.Add name ap rp tags -> do
        stmt <- DB.setStmt db "insert into article_info values (0, ?, ?, ?)"
        let values = map (DB.string2DB . T.pack) [ap, rp, name]
        DB.sendStmt db stmt values 
        putStrLn "add to database!"
        return (True, Lib.Command)
    Lib.Close -> return (True, Lib.Quit)
    Lib.Display args -> case args of 
        [] -> do 
            (defs, info) <- DB.queryRaw db "select * from article_info"
            flip fix info \loop stream -> 
                ST.read stream >>= \x -> case x of 
                    Just datas ->  do 
                        putStrLn $ unwords $ map (T.unpack . DB.fromVal) $ tail  datas
                        loop stream
                    Nothing -> return (True, Lib.Command)
        (name:empty) -> do
            stmt <- DB.setStmt db "select * from article_info where name = ?"
            (defs, info) <- DB.sendQuery db stmt [DB.string2DB $ T.pack name]
            flip fix info \loop stream -> 
                ST.read stream >>= \x -> case x of 
                    Just datas ->  do 
                        putStrLn $ unwords $ map (T.unpack . DB.fromVal) $ tail  datas
                        loop stream
                    Nothing -> return (True, Lib.Command)
    Lib.Edit name -> do 
        stmt <- DB.setStmt db "select readme_path from article_info where name = ?"
        (_, info) <- DB.sendQuery db stmt [DB.string2DB $ T.pack name]
        ST.read info >>= \d -> case d of 
            Just datas ->  do 
                (_, _, _, ph) <- createProcess (proc "/usr/bin/nvim" $ map (T.unpack . DB.fromVal) datas)
                exit <- waitForProcess ph
                putStrLn "edit finish!"
                return (True, Lib.Command)
            Nothing -> do 
                putStrLn "can't find such article"
                return (True, Lib.Command)
    Lib.RdDir options path -> do 
        contents <- filter (\s -> s /= "." && s /= "..") <$> getDirectoryContents path
        putStr  "if you need some tags then put bellow(tags are added to each read articles)\n"
        tags <- words <$> MG.getLine
        if Lib.OneDirectory  `elem` options then do
            let (h,t) = (head contents, head (tail contents))
                (article, readme) = if last h == 'f' then (path ++ h, path ++ t) else (path ++ t, path ++ h) 
                ad = Lib.mkData article readme tags
            putStrLn "article name?"
            name <- MG.getLine
            stmt <- DB.setStmt db "insert into article_info values (0, ?, ?, ?)"
            let args = map (DB.string2DB . T.pack) [article, readme, name] 
            DB.sendStmt db stmt args 
            return (True, Lib.Command)
        else do 
                let eggOfAds= flip map contents \name ->
                        let 
                            article = path  ++ name ++ "/article.pdf"
                            readme = path ++ name ++ "/readme.md"
                        in
                            (name, article, readme)
                flip fix eggOfAds \loop list ->
                    if null list then 
                        return (True, Lib.Command)
                    else do 
                        stmt <- DB.setStmt db "insert into article_info values (0, ?, ?, ?)"
                        let (name, article, readme) = head list 
                            args = map (DB.string2DB . T.pack) [article, readme, name] 
                        DB.sendStmt db stmt args
                        loop (tail list)
    Lib.Help -> do 
        putStrLn "Help will be implemented. Please wait"
        return (True, Lib.Command)
    Lib.Delete _ -> do 
        putStrLn "Delete will be implemented. Please wait"
        return (True, Lib.Command)

                



        
        
    
-- old ones ---------------------------------------
-- old interface 
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

-- old interface 
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
    mc <- CP.parseCommand <$> MG.getLine
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
registerCommand = Lib.parseCommand <$> MG.getLine
-}

-- old version executeCommand
-- main logic for command
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
    tags <- words <$> MG.getLine
    if Lib.OneDirectory  `elem` options then do
        let article = path ++ "/" ++ head contents
            readme = path ++ "/" ++ head (tail contents)
            ad = Lib.mkData article readme tags
        putStrLn "article name?"
        name <- MG.getLine
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

executeCommand Lib.Help b = do 
    -- todo
    return (True, Lib.Command , b)



initDatabase :: String -> IO Lib.Database
initDatabase file| null file = return Lib.emptyDatabase
                 | otherwise = return $ Lib.parseContainerFile file 
