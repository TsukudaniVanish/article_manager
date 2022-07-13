{-#LANGUAGE OverloadedStrings #-}
module SQLHandler where 

import Database.MySQL.Base
import qualified Data.ByteString.UTF8 as BSU

myConnectInfo :: String -> String -> String -> ConnectInfo 
myConnectInfo user pass db = defaultConnectInfo {ciUser = BSU.fromString user, ciPassword = BSU.fromString pass, ciDatabase = BSU.fromString db}

connectDB :: ConnectInfo -> IO MySQLConn
connectDB = connect

closeDB = close 

type DB = MySQLConn
type Val = MySQLValue

queryRaw = query_

setStmt = prepareStmt

sendQuery = queryStmt

sendStmt = executeStmt

string2DB = MySQLText

fromVal (MySQLText s) = s