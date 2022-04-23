{-#LANGUAGE OverloadedStrings #-}
module SQLHandler where 

import Database.MySQL.Base

myConnectInfo :: ConnectInfo 
myConnectInfo = defaultConnectInfo {ciUser = "root", ciPassword = "yuya", ciDatabase = "article"}

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