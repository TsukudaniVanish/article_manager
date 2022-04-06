{-
    if you need to add command, command options or edit command logic then edit this file.
    if you use this module in any situation except an unit test, 
    you should not call functions in this file except parseCommand'.
-}
{-#LANGUAGE OverloadedStrings #-}
module CommandParser where

import Data.Void(Void)
import Text.Megaparsec 
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Lib(ValidCommand(..), ValidOption(..))

type Parser a = Parsec Void String a

    --  Add String FilePath FilePath [String] -- name filepath readme-filepath tags
    --  Delete String -- name 
    --  Display [String] -- [] -> display all else display article-data connected with names in list 
    --  Close 
    --  RdDir String [String] -- dir-path options
    --  Edit String -- name
    -- Help

skipSpace :: Parser () 
skipSpace = L.space 
    -- one white space or many 
    space1
    -- line comment 
    (L.skipLineComment "//")
    --block comment 
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a 
lexeme = L.lexeme skipSpace

-- parse identifier name : rgexp  [a-zA-Z_#][a-zA-Z0-9_-.]*
readName :: Parser String 
readName = do 
    first <- letterChar <|> char '_' <|> char '#'
    second <- many $ alphaNumChar <|> char '_' <|> char '-' <|> char '.'
    pure $ first : second

-- parse full path : rgexp /([a-zA-Z0-9_]/)+[a-zA-Z0-9_]
readFilePathUb :: Parser FilePath 
readFilePathUb =
    concat <$> 
        some( do 
            char '/'
            s <- some (alphaNumChar <|> char '_' <|> char '.' <|> char '"' <|> char '\'' <|> char '-')
            pure $ '/' :  s
        )

readDirPathUb :: Parser FilePath 
readDirPathUb = do 
    char '/'
    res <- concat <$> some( do 
            s <- some (alphaNumChar <|> char '_' <|> char '.' <|> char '"' <|> char '\'' <|> char '-')
            char '/'
            pure $ s <> ['/']
        )
    pure ('/': res) 

readFilePath :: Parser FilePath
readFilePath = readFilePathUb

readDirPath :: Parser FilePath
readDirPath = readDirPathUb

readoptions :: Parser ValidOption
readoptions = label "options" $ do 
    char '-'
    n <- const 1 <$> string "one"
    case n of 
        1 -> pure OneDirectory


-- main logics for command parse
addcommand :: Parser ValidCommand
addcommand = label "add name file-path readme-file-path tags" $ do 
    lexeme $ string "add"
    name <- lexeme $ readName
    file <- lexeme $ readFilePath
    readme <- lexeme $ readFilePath 
    tags <- some (lexeme $ readName)
    pure (Add name file readme tags)

deletecommand :: Parser ValidCommand 
deletecommand = label "delete name" $ do 
    lexeme $ string "delete"
    Delete <$> readName 

displaycommand :: Parser ValidCommand 
displaycommand = label "display name, display" $ do 
    lexeme $ string "display" 
    Display <$> many (lexeme readName)

closecommand :: Parser ValidCommand 
closecommand = label "close" $ lexeme (string "close" >>= const (pure Close))

rddircommand :: Parser ValidCommand 
rddircommand = label "rddir -[options] path -[options]" $ do 
    lexeme $ string "rddir"
    optionsPfx <- many $ lexeme readoptions
    name <- lexeme readDirPath  
    optionsSfx <- many $ lexeme readoptions 
    pure $ RdDir (optionsPfx ++ optionsSfx) name

editcommand :: Parser ValidCommand 
editcommand = label "edit name" $ do 
    lexeme $ string "edit" 
    Edit <$> readName

helpcommand :: Parser ValidCommand 
helpcommand = label "help" $ do 
    lexeme $ string "help"
    pure Help 

commandParser :: Parser ValidCommand 
commandParser = 
    let mainPart = 
            lexeme $ choice 
                [ addcommand
                , deletecommand
                , displaycommand
                , closecommand
                , rddircommand
                , editcommand
                , helpcommand
                ]
    in between skipSpace eof mainPart

parseCommand' :: String -> Either (ParseErrorBundle String Void) ValidCommand 
parseCommand' = parse commandParser "" -- <- "" is name of source file
