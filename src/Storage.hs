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

del :: Maybe String -> (Maybe String, Maybe String) -> IO Bool
del s (u,p) = do
  db <- openDB
  let sf x = case s of Nothing -> True ; Just k -> k == x
  let uf y = case u of Nothing -> True ; Just k -> k == y
  let pf z = case p of Nothing -> True ; Just k -> k == z
  let db' = filter (\(x,(y,z)) -> sf x && uf y && pf z) db
  writeDB $ db \\ db'
  return $ length db' > 0
