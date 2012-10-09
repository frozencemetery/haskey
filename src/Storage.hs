module Storage (listEntries, get, add, del, showdbent) where

import Data.List
import System.IO

-- (service, (username, password))
type DBent = (String, (String, String))
type DB = [DBent]

showdbent :: DBent -> String
showdbent (s, (u,p)) = 
  concat ["Service:  ", s, "\nUsername: ", u, "\nPassword: ", p]

writeDB :: DB -> FilePath -> IO ()
writeDB db dblocat = do
  handle <- openFile dblocat WriteMode
  hPutStr handle $ show db
  hClose handle

openDB :: FilePath -> IO DB
openDB dblocat = do
  handle <- openFile dblocat ReadMode
  cont <- hGetLine handle
  hClose handle
  return $ read cont

listEntries :: FilePath -> IO String
listEntries dblocat = do
  db <- openDB dblocat
  let db' = intercalate "\n" $ map fst db
  return db'

get :: FilePath -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe DBent)
get dblocat s u p = do
  db <- openDB dblocat
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
add :: FilePath -> String -> String -> String -> IO Bool
add dblocat s u p = do
  db <- openDB dblocat
  let (b, db') = partition (\x -> fst x == s) db
  let newdb = (s, (u, p)) : db'
  writeDB newdb dblocat
  return $ length b == 1

del :: FilePath -> Maybe String -> Maybe String -> Maybe String -> IO Bool
del dblocat s u p = do
  db <- openDB dblocat
  let sf x = case s of Nothing -> True ; Just k -> k /= x
  let uf y = case u of Nothing -> True ; Just k -> k /= y
  let pf z = case p of Nothing -> True ; Just k -> k /= z
  let db' = filter (\(x,(y,z)) -> sf x && uf y && pf z) db
  writeDB db' dblocat
  return $ length db - length db' > 0
