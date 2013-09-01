module Crypt where

import Codec.Encryption.AES
import Codec.Encryption.Modes
import Codec.Encryption.Padding
import Data.Digest.SHA512
import Data.LargeWord
import System.Random

-- `unsafeCoerce' is used to convert between `Char's and `Word8's.

import Unsafe.Coerce

type Key = Word256
type Data = [Word128]

makeKey :: String -> Key

-- The security of `encryptMessage' depends on the security of the current
-- default system random generator.

encryptMessage :: Key -> String -> IO Data

-- `decryptMessage' can not determine whether it was given the correct key; if
-- the key given to it was not the key used to encrypt the data, it will return
-- garbage.

decryptMessage :: Key -> Data -> String

-------- --------

makeKey = sum.pkcs5.hash.(map unsafeCoerce)

encryptMessage k s =
  do iv <- fmap (head.pkcs5) $ sequence $ take 8 $ repeat randomIO
     return $ iv:(cbc encrypt iv k $ pkcs5 $ map unsafeCoerce s)

decryptMessage k (iv:m) = map unsafeCoerce $ unPkcs5 $ unCbc decrypt iv k m
