import Pwgen
import Storage
import Args
import System.Environment
import System.IO
import Control.Monad
import System.Random

version = "1.1hg"

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  (opts, trash) <- compilerOpts args home

  let dblocat = optDBlocat opts

  when (optShowVersion opts) $ print version
  when (optShowLicense opts) $ print "GPLv3 motherfuckers!"
  case optAction opts of
    Nothing -> return ()
    Just List ->
      do entries <- listEntries dblocat
         putStrLn entries
         return ()
    Just Lookup ->
      do entry <- get dblocat (optService opts) (optUser opts) (optPassword opts)
         let entry' = maybe "no entry found" showdbent entry
         putStrLn entry'
         return ()
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
                                 Just i -> return $ fst $ pwgen r i
                                 Nothing -> do putStr "Password: "
                                               hFlush stdout
                                               getLine
         b <- add dblocat sname uname pword
         case b of True -> putStrLn "Added."
                   False -> putStrLn "Added, overwriting existing entry."
         return ()
    Just Delete ->
      do killp <- del dblocat (optService opts) (optUser opts) (optPassword opts)
         case killp of True -> putStrLn "Deleted."
                       False -> putStrLn "No entries matched to delete."
         return ()
  return ()
