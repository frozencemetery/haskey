module Pwstore.Storage (listEntries,get,add) where

import Data.List
import System.IO

dblocat :: String
dblocat = "/home/frozencemetery/.pw.db"

-- (service, (username, password))
type DB = [(String, (String, String))]

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
--add :: String -> (String, String) -> Bool
add s r = do
  db <- openDB
  let (b, db') = partition (\(a, _) -> a == s) db
  let newdb = (s, r) : db'
  writeDB newdb
  return $ length b == 1

writeDB :: DB -> IO ()
writeDB db = do
  h <- openFile dblocat WriteMode
  hPutStrLn h $ show db
  hClose h


openDB :: IO DB
openDB = do
  -- cons <- readFile dblocat
  -- return $ read cons
  h <- openFile dblocat ReadMode
  db <- hGetContents h
  hClose h
  return $ read db
