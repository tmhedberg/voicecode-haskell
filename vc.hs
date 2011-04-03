import Control.Monad.State (execState, get, put)

import Data.Bits (shiftR, testBit, xor)
import Data.Char (ord)
import Data.List (splitAt)
import Data.Word (Word16, Word8)

import System.Environment (getArgs)

compute :: [Word8] -> Word16
compute bs = execState (mapM_ (\b -> get >>= \crc -> put (snd $ iterate crcIter (b, crc) !! 8)) bs) 0
    where crcIter (b, crc) = ( shiftR b 1
                             , (if testBit (b `xor` fromIntegral crc) 0 then (xor 0xA001) else id) (shiftR crc 1)
                             )

vc :: String -> String
vc s = show $ (fromIntegral . compute) (map (fromIntegral . ord) s) `mod` 10000

vcHyphen :: String -> String
vcHyphen = (\(a, b) -> a ++ "-" ++ b) . splitAt 2 . vc

main = getArgs >>= \(a:_) -> putStrLn (vcHyphen a)
