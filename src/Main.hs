import Args
import Control.Monad
import GetKey
import Prompts
import Pwgen
import Storage
import System.Environment
import System.IO
import System.Random
import XOut

version :: String
version = "1.2hg"

getDB :: Options -> String -> IO (String, DB)
getDB opt loc =
  case optKey opt of
    Nothing -> getDB' unlockPrompt loc
    Just k ->
      do dbm <- openDB k loc
         case dbm of
           Nothing -> error "Incorrect password."
           Just db -> return (k, db)
  where getDB' p dblocat =
          do k <- getKey p
             dbm <- openDB k dblocat
             case dbm of
               Nothing -> getDB' tryAgainPrompt dblocat
               Just db -> return (k, db)

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  (opts, _) <- compilerOpts args home

  let dblocat = optDBlocat opts

  when (optShowVersion opts) $ print version
  when (optShowLicense opts) $ print "GPLv3"

  case optAction opts of
    Nothing -> return ()
    Just List ->
      do (_, db) <- getDB opts dblocat
         entries <- listEntries db
         putStrLn entries
    Just Lookup ->
      do (_, db) <- getDB opts dblocat
         entry <- get db (optSelector opts) (optService opts) (optUser opts)
         let entry' = maybe "No entry found." showdbent entry
         let pword = case entry of Nothing -> ""; Just (_, _, p) -> p
         if optXOut opts then gen ":0" (optDelay opts) (optConfirm opts) 
                              (optReturn opts) pword
           else putStrLn entry'
    Just Create ->
      do (key, db) <- getDB opts dblocat
         sname <- case optService opts of Just k -> return k
                                          Nothing -> do putStr "Service:  "
                                                        hFlush stdout
                                                        getLine
         r <- newStdGen
         uname <- case optUser opts of
                    Just k -> return k
                    Nothing -> case optGenUser opts of
                                 Just i -> return $ fst $ pwgen r i
                                 Nothing -> do putStr "Username: "
                                               hFlush stdout
                                               getLine
         r' <- newStdGen
         pword <- case optPassword opts of
                    Just k -> return k
                    Nothing -> case optGenPw opts of
                                 Just i -> return $ fst $ pwgen r' i
                                 Nothing -> do putStr "Password: "
                                               hFlush stdout
                                               getLine
         b <- add key dblocat db sname uname pword
         case b of True -> putStrLn "Added, overwriting existing entry."
                   False -> putStrLn "Added."
    Just Delete ->
      do (key, db) <- getDB opts dblocat
         killp <- del key dblocat db (optSelector opts) (optService opts)
                  (optUser opts)
         case killp of True -> putStrLn "Deleted."
                       False -> putStrLn "No entries matched to delete."
    Just Rekey ->
      do (_, db) <- getDB opts dblocat
         newKey1 <- getKey newPasswordPrompt
         newKey2 <- getKey confirmPasswordPrompt
         if newKey1 == newKey2
           then writeDB newKey2 db dblocat
           else putStrLn $ "Sorry, new passwords did not match. "
                        ++ "No action performed."
    Just MakeDB ->
      do key <- getKey newPasswordPrompt
         makeDB key dblocat
