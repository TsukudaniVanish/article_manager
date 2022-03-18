{-#LANGUAGE MultiWayIf #-}
module Lib where

import qualified Data.HashMap as Map

import Data.Maybe

data UImode = Init | Command | Quit
data ValidCommand = Add String FilePath FilePath [String] | Delete String | Display | Close
    deriving (Eq, Show,Read)

-- containerfile format = (line ++ [NewLineChar])*
-- line = name++ containerFileSplitter ++ filePath++ containerFileSplitter ++ readmePath++ containerFileSplitter ++ tags
-- tags = ident(++ containerFileSplitter ++ ident)*

containerFileSplitter :: Char
containerFileSplitter = ',' 

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
dropLine s = 
    let (line, restWithNewLChar) = chopStringAtFirst newLineChar s
    in tail restWithNewLChar
         

getLn :: String -> String 
getLn s = 
    let (line, rest) = chopStringAtFirst newLineChar s in line

parseOneLine :: String -> Database -> Database
parseOneLine line base = 
    let tokens = line `splitBy` containerFileSplitter
        [name, filepath, readmepath] = take 3 tokens 
        tags = drop 3 tokens
        ad = Ad {filePath = filepath, readmePath = readmepath, tags = tags}
    in addDatabase name ad base