module GetKey where

import Data.IORef
import Graphics.UI.Gtk hiding (get, add)
import Prompts
import System.Exit

-- Interactively ask user for key
getKey :: String -> IO String
getKey p =
  do keyRef <- newIORef Nothing
     _ <- initGUI
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
     _ <- onClicked buttonOk $
       do t <- entryGetText entry
          writeIORef keyRef $ Just t
          mainQuit
     _ <- onClicked buttonCancel $ exitWith ExitSuccess
     _ <- onDestroy window mainQuit
     -- Go
     widgetShowAll window
     mainGUI
     key <- readIORef keyRef
     widgetHideAll window
     case key of
       Nothing -> getKey p
       Just k -> return k
