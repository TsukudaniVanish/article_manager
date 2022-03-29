{-#LANGUAGE MultiWayIf #-}
{-#LANGUAGE BlockArguments #-}
module Lib where

import qualified Data.HashMap as Map

import Data.Maybe

data UImode = Init | Command | Quit

data ValidOption 
    = OneDirectory
    deriving (Show, Eq, Read) 

data ValidCommand  
    = Add String FilePath FilePath [String] -- name filepath readme-filepath tags
    | Delete String -- name 
    | Display [String] -- [] -> display all else display article-data connected with names in list 
    | Close 
    -- | RdDir String [String]
    | RdDir [ValidOption] String  -- dir-path options
    | Edit String -- name 
    deriving (Eq, Show,Read)

-- containerFile format = (line ++ [NewLineChar])*
-- line = name++ containerFileSplitter ++ filePath++ containerFileSplitter ++ readmePath++ containerFileSplitter ++ tags
-- tags = ident(++ containerFileSplitter ++ ident)*

containerFileSplitter :: Char
containerFileSplitter = ',' 


{- 
    this is old version of parsers

parseCommand :: String -> Maybe ValidCommand
parseCommand s = let tokens = words s in 
    if null tokens then 
        Nothing 
    else 
        let instruction = head tokens
            operands = tail tokens
            in if | instruction == "add" -> readAdd operands
                  | instruction == "delete" -> readDelete operands
                  | instruction == "display" -> Just (Display operands)
                  | instruction == "close" -> Just Close
                  | instruction == "rddir" -> readRdDir operands
                  | instruction == "edit" -> readEdit operands
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

readRdDir :: [String] -> Maybe ValidCommand
readRdDir operands | length operands == 1 = let path = head operands in Just (RdDir path [])
                   | null operands = Nothing 
                   | otherwise  = Just (RdDir (head operands) (tail operands))

readEdit :: [String] -> Maybe ValidCommand
readEdit operands | length operands == 1 = let name = head operands in Just (Edit name) 
                  | otherwise = Nothing 
-}
type FPath = String
data ArticleData = Ad {
    filePath:: String,
    readmePath :: String,
    tags :: [String]
}
    deriving (Show, Eq)

emptyData :: ArticleData
emptyData = Ad { filePath= [], readmePath = [], tags = []}

mkData :: FPath  -> FPath  -> [String] -> ArticleData
mkData a r tags = Ad{filePath = a, readmePath = r, tags = tags}

type Database = Map.Map String ArticleData

emptyDatabase :: Database
emptyDatabase = Map.empty 

addDatabase :: String -> ArticleData -> Database -> Database
addDatabase  = Map.insert

deleteDatabase :: String -> Database -> Database
deleteDatabase = Map.delete  

getFromDatabase :: String -> Database -> ArticleData
getFromDatabase  = Map.findWithDefault emptyData 

showArticleData :: ArticleData -> String
showArticleData ad = "filePath: " ++ filePath ad ++ "\n" ++ "readme path: " ++ readmePath ad ++ "\n" ++ "tags: " ++ concatMap (++ " ") (tags ad)

displayData :: String -> ArticleData -> String
displayData key articleData = "article: " ++ key ++ "\n" ++ showArticleData articleData

displayDatabase :: Database -> String
displayDatabase b = unlines [ displayData key articleData| (key, articleData) <- Map.toList b ]

parseContainerFile :: String -> Database
parseContainerFile file = parse file emptyDatabase
    where parse :: String -> Database -> Database
          parse [] b = b
          parse file b = parse (dropLine file) (parseOneLine (getLn file) b)


newLineChar :: Char
newLineChar = '\n'

chopStringAtFirst :: Char -> String -> (String, String)
chopStringAtFirst c cs = 
    let index = findFirst c cs 0
        findFirst :: Num a =>  Char -> String -> a -> a 
        findFirst _c [] n = n
        findFirst _c (c:cs) n = if c == _c then n else findFirst _c cs $ n + 1
    in splitAt index cs   

splitBy :: String -> Char -> [String]
splitBy [] _ = []
splitBy s _c = 
    let (line, containsChar) = chopStringAtFirst _c s
    in if null containsChar then [line] else  line : tail containsChar `splitBy` _c

dropLine :: String -> String 
dropLine s| null s = []
          | otherwise = 
    let (line, restWithNewLChar) = chopStringAtFirst newLineChar s
    in tail restWithNewLChar
         

getLn :: String -> String 
getLn s = 
    let (line, rest) = chopStringAtFirst newLineChar s in line

parseOneLine :: String -> Database -> Database
parseOneLine line base = 
    let tokens = line `splitBy` containerFileSplitter
        [name, filepath, readmepath] = take 3 tokens 
        tags = filter (not . null) $ drop 3 tokens
        ad = Ad {filePath = filepath, readmePath = readmepath, tags = tags}
    in addDatabase name ad base

databaseToFileContent :: Database -> String 
databaseToFileContent b = 
    let contents = Map.toList b 
    in readAndWriteOneLines contents
        

dataToOneLine :: (String, ArticleData) -> String 
dataToOneLine (name, articleData) = concatMap (++ ",") $ [name, filePath articleData, readmePath articleData] ++ tags articleData 

readAndWriteOneLines :: [(String, ArticleData)] ->  String 
readAndWriteOneLines contents = unlines $ foldr helper [] contents
    where 
        helper content contents = dataToOneLine content : contents 
