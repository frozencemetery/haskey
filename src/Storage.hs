module Storage (listEntries,get,add) where

import Data.List
import System.IO

dblocat :: String
dblocat = "/home/frozencemetery/.pw.db"

-- (service, (username, password))
type DB = [(String, (String, String))]

writeDB :: DB -> IO ()
writeDB db = do
  handle <- openFile dblocat WriteMode
  hPutStr handle $ show db
  hClose handle

openDB :: IO DB
openDB = do
  handle <- openFile dblocat ReadMode
  cont <- hGetLine handle
  hClose handle
  return $ read cont

listEntries :: IO [String]
listEntries = do
  db <- openDB
  return $ map fst db

get :: String -> IO (Maybe (String, String))
get s = do
  db <- openDB
  return $ s `lookup` db

-- the Bool represents sharing
-- by which I mean whether it overwrote
add :: String -> (String, String) -> IO Bool
add s r = do
  db <- openDB
  let (b, db') = partition (\x -> fst x == s) db
  let newdb = (s, r) : db'
  writeDB newdb
  return $ length b == 1
