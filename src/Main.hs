import Pwgen
import Storage
import Args
import System.Environment
import System.IO
import Control.Monad

version = "0.5hg"

main :: IO ()
main = do
  args <- getArgs
  (opts, trash) <- compilerOpts args
  when (optShowVersion opts) $ print version
  when (optShowLicense opts) $ print "GPLv3 motherfuckers!"
  case optAction opts of
    Nothing -> return ()
    Just List -> 
      do entries <- listEntries 
         putStrLn entries
         return ()
    Just Lookup -> 
      do entry <- get (optServicename opts) (optUsername opts) (optPassword opts)
         let entry' = maybe "no entry found" showdbent entry
         putStrLn entry'
         return ()
    Just Create -> return ()
    Just Delete -> 
      do killp <- del (optServicename opts) (optUsername opts) (optPassword opts)
         case killp of True -> putStrLn "Deleted."
                       False -> putStrLn "No entries matched to delete."
         return ()
  return ()
