{-#LANGUAGE MultiWayIf #-}
module Lib where

import qualified Data.HashMap as Map

import Data.Maybe

data UImode = Init | Command | Quit
data ValidCommand = Add String FilePath FilePath [String] | Delete String | Display | Close
    deriving (Eq, Show,Read)

parseCommand :: String -> Maybe ValidCommand
parseCommand s = let tokens = words s in 
    if null tokens then 
        Nothing 
    else 
        let instruction = head tokens
            operands = tail tokens
            in if | instruction == "add" -> readAdd operands
                  | instruction == "delete" -> readDelete operands
                  | instruction == "display" -> Just Display
                  | instruction == "close" -> Just Close
                  | otherwise -> Nothing

readAdd :: [String] -> Maybe ValidCommand
readAdd operands| length operands < 3 = Nothing
                | length operands == 3 = let [name, apath, rpath] = operands in Just (Add name apath rpath [])
                | otherwise = let [name, apath, rpath] = take 3 operands
                                  tags = drop 3 operands in
                                      Just (Add name apath rpath tags)

readDelete :: [String] -> Maybe ValidCommand
readDelete operands | null operands = Nothing 
                    | otherwise = let name = head operands in Just (Delete name)





type FPath = String
data ArticleData = Ad {
    filePath:: String,
    readmePath :: String,
    tags :: [String]
}
emptyData :: ArticleData
emptyData = Ad { filePath= [], readmePath = [], tags = []}

mkData :: FPath  -> FPath  -> [String] -> ArticleData
mkData a r tags = Ad{filePath = a, readmePath = r, tags = tags}

type Database = Map.Map String ArticleData

initDatabase :: IO Database
initDatabase = return Map.empty

addDatabase :: String -> ArticleData -> Database -> IO Database
addDatabase key articleData base = return $ Map.insert key articleData base

getFromDatabase :: String -> Database -> IO ArticleData
getFromDatabase key base = return $ Map.findWithDefault emptyData key base

showArticleData :: ArticleData -> String
showArticleData ad = "filePath: " ++ filePath ad ++ "\n" ++ "readme path: " ++ readmePath ad ++ "\n" ++ "tags: " ++ concatMap (++ " ") (tags ad)

displayData :: String -> ArticleData -> String
displayData key articleData = "article: " ++ key ++ "\n" ++ showArticleData articleData

displayDatabase :: Database -> String
displayDatabase b = unlines [ displayData key articleData| (key, articleData) <- Map.toList b ]
