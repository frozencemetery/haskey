import Args
import Control.Monad
import Crypt
import Pwgen
import Storage
import System.Environment
import System.IO
import System.Random
import XOut

version = "1.2hg"
keyPrompt = "Enter keychain password: "
oldPrompt = "Enter current keychain password: "
newPrompt = "Enter new keychain password: "
confirmPrompt = "Confirm new keychain password: "

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  (opts, trash) <- compilerOpts args home

  let dblocat = optDBlocat opts

  when (optShowVersion opts) $ print version
  when (optShowLicense opts) $ print "GPLv3"

  let getKey p = case optKey opts of
        Nothing -> do putStr p
                      hFlush stdout
                      getLine
        Just k -> return k

  case optAction opts of
    Nothing -> return ()
    Just List ->
      do key <- getKey keyPrompt
         entries <- listEntries (makeKey key) dblocat
         putStrLn entries
    Just Lookup ->
      do key <- getKey keyPrompt
         entry <- get (makeKey key) dblocat (optService opts) (optUser opts)
                  (optPassword opts)
         let entry' = maybe "no entry found" showdbent entry
         let pword = case entry of Nothing -> ""; Just (s, u, p) -> p
         if optXOut opts then gen ":0" $ pword ++ "\n" else putStrLn entry'
    Just Create ->
      do key <- getKey keyPrompt
         sname <- case optService opts of Just k -> return k
                                          Nothing -> do putStr "Service:  "
                                                        hFlush stdout
                                                        a <- getLine
                                                        return a
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
         b <- add (makeKey key) dblocat sname uname pword
         case b of True -> putStrLn "Added, overwriting existing entry."
                   False -> putStrLn "Added."
    Just Delete ->
      do key <- getKey keyPrompt
         killp <- del (makeKey key) dblocat (optService opts) (optUser opts)
                      (optPassword opts)
         case killp of True -> putStrLn "Deleted."
                       False -> putStrLn "No entries matched to delete."
    Just Rekey ->
      do putStrLn $ "Warning: if you misstype your old password, "
                 ++ "all data will be lost."
         oldKey <- getKey oldPrompt
         newKey1 <- getKey newPrompt
         newKey2 <- getKey confirmPrompt
         if newKey1 == newKey2
           then rekey (makeKey oldKey) (makeKey newKey2) dblocat
           else putStrLn $ "Sorry, new passwords did not match. "
                        ++ "No action performed."
