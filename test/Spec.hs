import SrcTest(testAll)
import TestForParser(testCP)

main :: IO ()
main = do 
    testAll
    testCP
