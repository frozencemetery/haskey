import Args
import Control.Monad
import Crypt
import Data.IORef
import Graphics.UI.Gtk hiding (get, add)
import Pwgen
import Storage
import System.Environment
import System.IO
import System.Random
import System.Exit
import XOut

version = "1.2hg"
keyPrompt = "Enter keychain password: "
tryAgainPrompt = "Incorrect password. Please try again: "
oldPrompt = "Enter current keychain password: "
newPrompt = "Enter new keychain password: "
confirmPrompt = "Confirm new keychain password: "
title = "Password entry."

-- Interactively ask user for key
getKey :: String -> IO String
getKey p =
  do keyRef <- newIORef Nothing
     initGUI
     -- Widgets
     window <- windowNew
     label <- labelNew $ Just p
     buttonOk <- buttonNewWithLabel "Ok"
     buttonCancel <- buttonNewWithLabel "Cancel"
     entry <- entryNew
     hbox <- hBoxNew False 10
     -- Widget settings
     entrySetVisibility entry False
     windowSetKeepAbove window True
     containerAdd window hbox
     boxPackStart hbox label PackNatural 0
     boxPackStart hbox entry PackNatural 0
     boxPackStart hbox buttonOk PackNatural 0
     boxPackStart hbox buttonCancel PackNatural 0
     set window [ windowTitle := title
                , windowResizable := False
                , windowDefaultWidth := 400 ]
     -- Handlers
     onClicked buttonOk $
       do t <- entryGetText entry
          writeIORef keyRef $ Just t
          mainQuit
     onClicked buttonCancel $ exitWith ExitSuccess
     onDestroy window mainQuit
     -- Go
     widgetShowAll window
     mainGUI
     key <- readIORef keyRef
     widgetHideAll window
     case key of
       Nothing -> getKey p
       Just key -> return key

getDB :: String -> IO (Key, DB)
getDB = getDB' keyPrompt
  where getDB' p dblocat =
          do k <- getKey p
             dbm <- openDB (makeKey k) dblocat
             case dbm of
               Nothing -> getDB' tryAgainPrompt dblocat
               Just db -> return (makeKey k, db)

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  (opts, trash) <- compilerOpts args home

  let dblocat = optDBlocat opts

  when (optShowVersion opts) $ print version
  when (optShowLicense opts) $ print "GPLv3"

  case optAction opts of
    Nothing -> return ()
    Just List ->
      do (key, db) <- getDB dblocat
         entries <- listEntries db
         putStrLn entries
    Just Lookup ->
      do (key, db) <- getDB dblocat
         entry <- get db (optService opts) (optUser opts) (optPassword opts)
         let entry' = maybe "no entry found" showdbent entry
         let pword = case entry of Nothing -> ""; Just (s, u, p) -> p
         if optXOut opts then gen ":0" $ pword ++ "\n" else putStrLn entry'
    Just Create ->
      do (key, db) <- getDB dblocat
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
         b <- add key dblocat db sname uname pword
         case b of True -> putStrLn "Added, overwriting existing entry."
                   False -> putStrLn "Added."
    Just Delete ->
      do (key, db) <- getDB dblocat
         killp <- del key dblocat db (optService opts) (optUser opts)
                      (optPassword opts)
         case killp of True -> putStrLn "Deleted."
                       False -> putStrLn "No entries matched to delete."
    Just Rekey ->
      do (_, db) <- getDB dblocat
         newKey1 <- getKey newPrompt
         newKey2 <- getKey confirmPrompt
         if newKey1 == newKey2
           then writeDB (makeKey newKey2) db dblocat
           else putStrLn $ "Sorry, new passwords did not match. "
                        ++ "No action performed."

    Just MakeDB ->
      do key <- getKey newPrompt
         makeDB (makeKey key) dblocat
