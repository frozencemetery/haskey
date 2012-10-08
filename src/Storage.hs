module Storage (listEntries, get, add, del, showdbent) where

import Data.List
import System.IO

dblocat :: String
dblocat = "/home/frozencemetery/.pw.db"

-- (service, (username, password))
type DBent = (String, (String, String))
type DB = [DBent]

showdbent :: DBent -> String
showdbent (s, (u,p)) = 
  concat ["Service:  ", s, "\nUsername: ", u, "\nPassword: ", p]

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

listEntries :: IO String
listEntries = do
  db <- openDB
  let db' = concat $ intersperse "\n" $ map fst db
  return db'

get :: Maybe String -> Maybe String -> Maybe String -> IO (Maybe DBent)
get s u p = do
  db <- openDB
  let sf x = case s of Nothing -> True ; Just k -> k == x
  let uf y = case u of Nothing -> True ; Just k -> k == y
  let pf z = case p of Nothing -> True ; Just k -> k == z
  let db' = filter (\(x,(y,z)) -> sf x && uf y && pf z) db
  let ret = case length db' of
              1 -> Just $ head db'
              _ -> Nothing
  return ret

-- the Bool represents sharing
-- by which I mean whether it overwrote
add :: String -> (String, String) -> IO Bool
add s r = do
  db <- openDB
  let (b, db') = partition (\x -> fst x == s) db
  let newdb = (s, r) : db'
  writeDB newdb
  return $ length b == 1

del :: Maybe String -> Maybe String -> Maybe String -> IO Bool
del s u p = do
  db <- openDB
  let sf x = case s of Nothing -> True ; Just k -> k /= x
  let uf y = case u of Nothing -> True ; Just k -> k /= y
  let pf z = case p of Nothing -> True ; Just k -> k /= z
  let db' = filter (\(x,(y,z)) -> sf x && uf y && pf z) db
  writeDB db'
  return $ length db - length db' > 0
