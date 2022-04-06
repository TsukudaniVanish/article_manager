module TestForParser where

import Control.Applicative hiding (some)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import CommandParser
import Lib(ValidCommand(..), ValidOption (OneDirectory))

testCP = hspec $ 
    describe "CommandParser" $ do 
        it "addcommand" $ 
            parse commandParser "" "  add test12 /home/a.ps /home/readme/a.md #tag1 #tag2" 
                `shouldParse` Add "test12" "/home/a.ps" "/home/readme/a.md" ["#tag1", "#tag2"]
        it "deletecommand" $
            parse commandParser "" "delete name" `shouldParse` Delete "name" 
        it "displaycommand" $
            parse commandParser "" " display name" `shouldParse` Display ["name"]
        it "rddircommand" $
            parse commandParser "" "rddir /path/like/this/ -one" `shouldParse` RdDir [OneDirectory] "/path/like/this/"
        it "editcommand" $
            parse commandParser "" "edit name" `shouldParse` Edit "name"
        it "closecommand" $
            parse commandParser "" "close" `shouldParse` Close
        it "helpcommand" $ 
            parse commandParser "" "help " `shouldParse` Help 
        