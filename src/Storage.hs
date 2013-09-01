{-# LANGUAGE ScopedTypeVariables #-}

module Storage
  ( DB
  , listEntries
  , get
  , add
  , del
  , showdbent
  , makeDB
  , openDB
  , writeDB
  ) where

import Codec.Utils
import Crypt
import Data.Digest.SHA256
import Data.LargeWord
import Data.List
import System.IO

-- (service, username, password)
type DBent = (String, String, String)
type DB = [DBent]

showdbent :: DBent -> String
showdbent (s, u, p) =
  concat ["Service:  ", s, "\nUsername: ", u, "\nPassword: ", p]

writeDB :: Key -> DB -> FilePath -> IO ()
writeDB key db dblocat = do
  handle <- openFile dblocat WriteMode
  dbe <- encryptMessage key $ show db
  hPutStr handle $ show (hash (toOctets (256 :: Integer) key), dbe)
  hClose handle

openDB :: Key -> FilePath -> IO (Maybe DB)
openDB key dblocat = do
  handle <- openFile dblocat ReadMode
  cont <- hGetLine handle
  hClose handle
  let (check, dbi) = read cont :: ([Octet], [Integer])
  if check == hash (toOctets (256 :: Integer) key)
    then return $ Just $ read $ decryptMessage key $ map fromInteger dbi
    else return Nothing

listEntries :: DB -> IO String
listEntries db = do
  let db' = intercalate "\n" $ map (\(x,y,z) -> x) db
  return db'

get :: DB -> Maybe String -> Maybe String -> Maybe String
       -> IO (Maybe DBent)
get db s u p = do
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
add :: Key -> FilePath -> DB -> String -> String -> String -> IO Bool
add key dblocat db s u p = do
  let (b, db') = partition (\(x,y,z) -> x == s) db
  let newdb = (s, u, p) : db'
  writeDB key newdb dblocat
  return $ length b == 1

del :: Key -> FilePath -> DB -> Maybe String -> Maybe String -> Maybe String
       -> IO Bool
del key dblocat db s u p = do
  let sf x = case s of Nothing -> True ; Just k -> k /= x
  let uf y = case u of Nothing -> True ; Just k -> k /= y
  let pf z = case p of Nothing -> True ; Just k -> k /= z
  let db' = filter (\(x,y,z) -> sf x && uf y && pf z) db
  writeDB key db' dblocat
  return $ length db - length db' > 0

makeDB :: Key -> FilePath -> IO ()
makeDB key dblocat = writeDB key [] dblocat
