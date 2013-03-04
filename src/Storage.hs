{-# LANGUAGE ScopedTypeVariables #-}

module Storage (listEntries, get, add, del, showdbent) where

import Crypt
import Data.LargeWord
import Data.List
import System.IO
import Data.Functor

type DBent = (String, String, String)
type DB = [DBent]

showdbent :: DBent -> String
showdbent (s, u, p) =
  concat ["Service:  ", s, "\nUsername: ", u, "\nPassword: ", p]

writeDB :: Key -> DB -> FilePath -> IO ()
writeDB key db dblocat = do
  handle <- openFile dblocat WriteMode
  dbe <- mapM ((encryptMessage key).show) db
  hPutStr handle $ show dbe
  hClose handle

openDB :: Key -> FilePath -> IO DB
openDB key dblocat = do
  handle <- openFile dblocat ReadMode
  cont <- hGetLine handle
  hClose handle
  return $ read.(decryptMessage key) <$> map fromInteger <$> read cont

listEntries :: Key -> FilePath -> IO String
listEntries key dblocat = do
  db <- openDB key dblocat
  let db' = intercalate "\n" $ map (\(x,y,z) -> x) db
  return db'

get :: Key -> FilePath -> Maybe String -> Maybe String -> Maybe String
       -> IO (Maybe DBent)
get key dblocat s u p = do
  db <- openDB key dblocat
  let sf x = case s of Nothing -> True ; Just k -> k == x
  let uf y = case u of Nothing -> True ; Just k -> k == y
  let pf z = case p of Nothing -> True ; Just k -> k == z
  let db' = filter (\(x,y,z) -> sf x && uf y && pf z) db
  let ret = case length db' of
              1 -> Just $ head db'
              _ -> Nothing
  return ret

-- the Bool represents sharing
-- by which I mean whether it overwrote
add :: Key -> FilePath -> String -> String -> String -> IO Bool
add key dblocat s u p = do
  db <- openDB key dblocat
  let (b, db') = partition (\(x,y,z) -> x == s) db
  let newdb = (s, u, p) : db'
  writeDB key newdb dblocat
  return $ length b == 1

del :: Key -> FilePath -> Maybe String -> Maybe String -> Maybe String -> IO Bool
del key dblocat s u p = do
  db <- openDB key dblocat
  let sf x = case s of Nothing -> True ; Just k -> k /= x
  let uf y = case u of Nothing -> True ; Just k -> k /= y
  let pf z = case p of Nothing -> True ; Just k -> k /= z
  let db' = filter (\(x,y,z) -> sf x && uf y && pf z) db
  writeDB key db' dblocat
  return $ length db - length db' > 0
