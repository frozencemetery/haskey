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

import Control.DeepSeq
import Data.List
import GHC.IO.Exception
import System.IO
import System.Process
import System.Posix.IO

type Key = String

-- (service, username, password)
type DBent = (String, String, String)
type DB = [DBent]

showdbent :: DBent -> String
showdbent (s, u, p) =
  concat ["Service:  ", s, "\nUsername: ", u, "\nPassword: ", p]

writeDB :: Key -> DB -> FilePath -> IO ()
writeDB key db dblocat =
  do (x, y) <- createPipe
     h <- fdToHandle y
     outHandle <- openFile dblocat WriteMode
     let cp = CreateProcess
           { cmdspec = RawCommand "openssl" ["enc", "-aes-256-cbc", "-pass",
                                             "fd:" ++ show x]
           , cwd = Nothing
           , env = Just []
           , std_in = CreatePipe
           , std_out = UseHandle outHandle
           , std_err = Inherit
           , close_fds = False
           , create_group = False }
     (Just i, Nothing, Nothing, p) <- createProcess cp
     hPutStrLn i $ show db
     hClose i
     hPutStr h $ key ++ "\n"
     hClose h
     ec <- waitForProcess p
     case ec of
       ExitSuccess -> hClose outHandle
       ExitFailure _ -> error "Fatal error: openssl died."

openDB :: Key -> FilePath -> IO (Maybe DB)
openDB key dblocat =
  do (x, y) <- createPipe
     h <- fdToHandle y
     inHandle <- openFile dblocat ReadMode
     let cp = CreateProcess
           { cmdspec = RawCommand "openssl" ["enc", "-d", "-aes-256-cbc",
                                             "-pass", "fd:" ++ show x]
           , cwd = Nothing
           , env = Just []
           , std_in = UseHandle inHandle
           , std_out = CreatePipe
           , std_err = Inherit
           , close_fds = False
           , create_group = False }
     (Nothing, Just o, Nothing, p) <- createProcess cp
     hPutStr h $ key ++ "\n"
     hClose h
     ec <- waitForProcess p
     hClose inHandle
     case ec of
       ExitSuccess -> do dbStr <- hGetContents o
                         deepseq dbStr $ hClose o
                         return $ Just $ read dbStr
       ExitFailure 1 -> do hClose o
                           return Nothing
       ExitFailure _ -> error "Fatal error: openssl died."

listEntries :: DB -> IO String
listEntries db = do
  let db' = intercalate "\n" $ map (\(x,_,_) -> x) db
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
  let (b, db') = partition (\(x,_,_) -> x == s) db
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
