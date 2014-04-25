module GetKey where

import Control.Concurrent
import Graphics.UI.Gtk
import System.Exit
import System.IO

import qualified Prompts as P

-- Most of this code is transliterated from libgksu.
-- * Copyright (C) 2004-2009 Gustavo Noronha Silva
-- * Portions Copyright (C) 2009 VMware, Inc.

grab :: Int -> IO GrabStatus -> IO ()
grab 0 _ = hPutStrLn stderr P.focusFail
grab n f =
  do status <- f
     case status of
       GrabSuccess -> return ()
       _ ->
         do hPutStrLn stderr P.focusRetry
            threadDelay 250000
            grab (n - 1) f

loop :: IO ()
loop = do b <- eventsPending
          if b > 0 then do _ <- mainIteration; loop
            else return ()

getKey :: P.Prompt -> IO String
getKey p =
  do _ <- initGUI
     dialog <- dialogNew
     vbox <- dialogGetUpper dialog

     windowSetTitle dialog ""
     dialogSetHasSeparator dialog False
     windowSetFrameDimensions dialog 6 6 6 6
     windowSetDecorated dialog False
     boxSetSpacing vbox 12
     windowSetResizable dialog False
     windowSetSkipPagerHint dialog True
     windowSetSkipTaskbarHint dialog True
     windowSetPosition dialog WinPosCenter

     _ <- dialogAddButton dialog stockCancel ResponseCancel
     okButton <- dialogAddButton dialog stockOk ResponseOk
     widgetGrabDefault okButton

     hbox <- hBoxNew False 12
     containerSetBorderWidth hbox 6
     boxPackStart vbox hbox PackGrow 0
     widgetShow hbox

     image <- imageNewFromStock stockDialogAuthentication IconSizeDialog
     miscSetAlignment image 0.5 0
     boxPackStart hbox image PackNatural 0
     widgetShow image

     entryVbox <- vBoxNew False 12
     boxPackStart hbox entryVbox PackGrow 0
     widgetShow entryVbox

     label <- labelNew
       $ Just $ "<span weight=\"bold\" size=\"larger\">" ++ P.message p
         ++ "</span>\n"
     labelSetUseMarkup label True
     labelSetLineWrap label True
     miscSetAlignment label 0.0 0
     boxPackStart entryVbox label PackGrow 0
     widgetShow label

     alert <- labelNew $ Just $ P.alert p
     boxPackStart entryVbox alert PackGrow 0
     widgetShow alert

     entryHbox <- hBoxNew False 6
     boxPackStart entryVbox entryHbox PackGrow 0
     widgetShow entryHbox

     promptLabel <- labelNew $ Just "Password:"
     boxPackStart entryHbox promptLabel PackNatural 0
     widgetShow promptLabel

     entry <- entryNew
     -- TODO(sjindel): key-press-event signal for capslock indicator
     _ <- onEntryActivate entry (buttonClicked okButton)
     entrySetVisibility entry False
     boxPackStart entryHbox entry PackGrow 0
     widgetShow entry
     widgetGrabFocus entry

     labelWarnCapsLock <- labelNew $ Just ""
     widgetShow labelWarnCapsLock

     labelSetJustify labelWarnCapsLock JustifyCenter
     labelSetUseMarkup labelWarnCapsLock True
     boxPackStart entryVbox labelWarnCapsLock PackGrow 0

     -- TODO(sjindel): Verify capslock on focus-in-event.

     -- TODO(sjindel): Dim screen.
     widgetShowAll dialog
     dialogCursor <- cursorNew LeftPtr
     dw <- widgetGetDrawWindow dialog

     -- This loop isn't strictly necessary, but causes the window to be
     -- created sooner on some systems.
     loop

     grab 16 $ pointerGrab dw True [] (Nothing :: Maybe DrawWindow)
       (Just dialogCursor) currentTime
     grab 16 $ keyboardGrab dw True currentTime
     windowSetKeepAbove dialog True

     loop

     rid <- dialogRun dialog
     widgetHide dialog
     pointerUngrab currentTime
     keyboardUngrab currentTime
     flush

     loop

     case rid of
       ResponseOk ->
         do key <- entryGetText entry; widgetDestroy dialog; loop; return key
       _ -> exitWith $ ExitFailure 2
