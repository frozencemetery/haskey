module Pwstore.Pwgen where

import System.Random

srcstring :: String
srcstring = "`1234567890-=~!@#$%^&*()_+qwertyuiop[]\\QWERTYUIOP{}|asdfghjkl;'"
            ++ "ASDFGHJKL:\"zxcvbnm,./ZXCVBNM<>?"

pwgen :: (RandomGen g) => g -> Int -> (String, g)
pwgen g i = 
  let p :: (RandomGen g) => g -> Int -> String -> (String, g)
      p g 0 acc = (acc, g)
      p g i acc = p g' (i-1) (srcstring !! n : acc) 
        where (n, g') = randomR (0, length(srcstring)-1) g
  in p g i ""

pwgend :: (RandomGen g) => g -> (String, g)
pwgend = \g -> pwgen g 128

-- For testing purposes only
main = do
  r <- newStdGen
  putStrLn $ fst $ pwgend r
