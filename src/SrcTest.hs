module SrcTest where

import Lib

import Data.Maybe 

strictEq :: Eq a => Maybe a -> Maybe a -> Bool
strictEq (Just x) (Just y) = x == y
strictEq _ _ = False 

showInGreen :: String -> String
showInGreen s = "\x1b[32m" ++ s ++ "\x1b[m"


testTemp :: String -> Bool -> IO ()
testTemp test  p= if p then putStrLn $ showInGreen $ test ++ " passed!" else error $ test ++ " failed"

testAll :: IO ()
testAll = do testParseCommand
             testGetLine
             testSplitBy
             testDropLine

testParseCommand = do let testName = "parseCommand"
                          no = (testName ++) .show 
                          test num p = testTemp (no num) p    
                          first = parseCommand "add article ~/a/something ~/b/readme.md #notag #thisIsTest" `strictEq` Just (Add "article" "~/a/something" "~/b/readme.md" ["#notag", "#thisIsTest"])
                          second = isNothing $ parseCommand "ssnjbvbu"
                          third = parseCommand "close" `strictEq` Just Close 
                      test 1 first
                      test 2 second
                      test 3 third

testGetLine = testTemp "getLn" (getLn "abc\nthisisfail" == "abc" && getLn "abs" == "abs") 

testSplitBy = testTemp "splitBy" (splitBy "a b c" ' ' == ["a", "b", "c"])

testDropLine = testTemp "dropLine" (dropLine "fail\nOk" == "Ok" && dropLine "\nOk" == "Ok")