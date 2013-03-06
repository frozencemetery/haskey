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
oldPrompt = "Warning: if you mistype your password, all data will be lost. "
         ++ "Enter current keychain password: "
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
     case key of
       Nothing -> getKey p
       Just key -> return key

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
      do oldKey <- getKey oldPrompt
         newKey1 <- getKey newPrompt
         newKey2 <- getKey confirmPrompt
         if newKey1 == newKey2
           then rekey (makeKey oldKey) (makeKey newKey2) dblocat
           else putStrLn $ "Sorry, new passwords did not match. "
                        ++ "No action performed."
