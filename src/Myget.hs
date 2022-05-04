{-#LANGUAGE BlockArguments#-}
module Myget where
import Data.Function
import Control.Monad
getLine :: IO String 
getLine = flip fix [] \loop str ->
    do 
        c <- getChar 
        case c of 
            _| c =='\n' -> return $ reverse str
            _| c == '\DEL' -> do 
                putStr "\ESC[2K"
                putChar '\r'
                let str' = if null str then str else tail str
                putStr $ reverse str'
                loop str'
            _| c == '\ESC' -> do
                replicateM_ 2 getChar
                putStr "\ESC[2K"
                putChar '\r'
                putStr $ reverse str 
                loop str 
            _| otherwise -> loop (c:str)