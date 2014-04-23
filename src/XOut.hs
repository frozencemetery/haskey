module XOut where

import Control.Concurrent
import Control.Monad
import Data.List
import Graphics.X11
import Graphics.X11.XTest

mapkey :: Char -> ([KeySym], KeySym)
mapkey '`' = ([], xK_quoteleft)
mapkey '-' = ([], xK_minus)
mapkey '=' = ([], xK_equal)
mapkey '~' = ([xK_Shift_R], xK_asciitilde)
mapkey '!' = ([xK_Shift_R], xK_exclam)
mapkey '@' = ([xK_Shift_R], xK_at)
mapkey '#' = ([xK_Shift_R], xK_numbersign)
mapkey '$' = ([xK_Shift_R], xK_dollar)
mapkey '%' = ([xK_Shift_R], xK_percent)
mapkey '^' = ([xK_Shift_R], xK_asciicircum)
mapkey '&' = ([xK_Shift_R], xK_ampersand)
mapkey '*' = ([xK_Shift_R], xK_asterisk)
mapkey '_' = ([xK_Shift_R], xK_underscore)
mapkey '+' = ([xK_Shift_R], xK_plus)
mapkey '{' = ([xK_Shift_R], xK_bracketleft)
mapkey '}' = ([xK_Shift_R], xK_bracketright)
mapkey '[' = ([], xK_bracketleft)
mapkey ']' = ([], xK_bracketright)
mapkey '\\' = ([], xK_backslash)
mapkey '|' = ([xK_Shift_R], xK_bar)
mapkey ';' = ([], xK_semicolon)
mapkey ':' = ([xK_Shift_R], xK_colon)
mapkey '\'' = ([], xK_apostrophe)
mapkey '"' = ([xK_Shift_R], xK_apostrophe)
mapkey ',' = ([], xK_comma)
mapkey '.' = ([], xK_period)
mapkey '/' = ([], xK_slash)
mapkey '?' = ([xK_Shift_R], xK_question)
mapkey ' ' = ([], xK_space)
mapkey '\n' = ([], xK_Return)
mapkey '\t' = ([], xK_Tab)
mapkey '>' = ([xK_Shift_R], xK_greater)
mapkey '<' = ([], xK_less) -- no shift here is correct (why?)
-- doing this with parenleft breaks qemu integration
-- likewise with parenright
mapkey '(' = ([xK_Shift_R], xK_9)
mapkey ')' = ([xK_Shift_R], xK_0)
mapkey c
  | c `elem` letters = let Just i = elemIndex c letters
                       in ([], keys !! i)
  | c `elem` capitals = let Just i = elemIndex c capitals
                        in ([xK_Shift_R], keys !! i)
  | c `elem` numbers = let Just i = elemIndex c numbers
                       in ([], numkeys !! i)
    where letters = ['a'..'z']
          capitals = ['A'..'Z']
          keys = [xK_a..xK_z]
          numbers = ['0'..'9']
          numkeys = [xK_0..xK_9]
mapkey c = error $ "No map for key: " ++ [c]

gen :: String  -- ^ screen to output to
       -> Int  -- ^ delay before typing (in milliseconds)
       -> Bool  -- ^ whether to use confirm mode
       -> Bool  -- ^ whether to press return after typing the password
       -> String  -- ^ password
       -> IO ()
gen display delay confirm ret str =
  do threadDelay (delay * 1000)
     display' <- openDisplay display
     let keys = sequence_ $ intersperse (threadDelay 1000) 
                $ map (uncurry (sendKey display') . mapkey) str
     Just _ <- queryXTestSupport display' -- this line
     keys
     when confirm $ uncurry (sendKey display') (mapkey '\t') >> keys
     when ret $ uncurry (sendKey display') (mapkey '\n')
