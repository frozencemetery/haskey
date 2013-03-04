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

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  (opts, trash) <- compilerOpts args home

  let dblocat = optDBlocat opts

  when (optShowVersion opts) $ print version
  when (optShowLicense opts) $ print "GPLv3"

  key <- case optKey opts of
    Nothing -> do putStr "Enter keychain password: "
                  hFlush stdout
                  getLine
    Just k -> return k

  case optAction opts of
    Nothing -> return ()
    Just List ->
      do entries <- listEntries (makeKey key) dblocat
         putStrLn entries
    Just Lookup ->
      do entry <- get (makeKey key) dblocat (optService opts) (optUser opts)
                  (optPassword opts)
         let entry' = maybe "no entry found" showdbent entry
         let pword = case entry of Nothing -> ""; Just (s, u, p) -> p
         if optXOut opts then gen ":0" $ pword ++ "\n" else putStrLn entry'
    Just Create ->
      do sname <- case optService opts of Just k -> return k
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
         return ()
    Just Delete ->
      do killp <- del (makeKey key) dblocat (optService opts) (optUser opts) (optPassword opts)
         case killp of True -> putStrLn "Deleted."
                       False -> putStrLn "No entries matched to delete."
