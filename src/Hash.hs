{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -Woverflowed-literals #-}

module Hash (testHashes) where

import Data.Word
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Data.Bits
import System.IO
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import System.Random
import qualified Data.Map as Map

type HashFun = [Word8] -> Word8

testHashes :: BS.ByteString -> IO String
testHashes file0 = do
  let file = BS.unpack file0
  n <- randomRIO (0, length file - 1)
  let file' = modifyAt n (`complementBit` 0) file
  foldM (\s (name, f) ->
    let (h1, h2, delta) = testHash f file file'
    in return $ s ++"<br/>("++ name ++")"
       ++ " h1:" ++ h1
       ++", h2:"++ h2
       ++", Δ:" ++ show delta)
    ("Modified byte number " ++ show n)
    (Map.toList hashes)
    

main :: IO ()
main = do
  (file, file') <- prepareFiles "bcomp.v"
  print $ testHash hash_adler file file'
  putStrLn "OK"

test :: IO ()
test = do
  (file, file') <- prepareFiles "mih.jpeg"     
  forM_ (Map.toList hashes) (\(name, f) ->
    putStrLn $ name ++ ": " ++ show (testHash f file file'))  
  putStrLn "OK"

-- Utility functions

prepareFiles :: FilePath -> IO ([Word8], [Word8])
prepareFiles path = do
  file <- BS.unpack <$> BS.readFile path
  n <- randomRIO (0, length file - 1)
  let file' = modifyAt n (`complementBit` 0) file
  putStrLn $ "Modified byte number " ++ show n
  return (file, file')

testHash fn f1 f2 = (h1, h2, h1 `diff` h2)
  where
    h1 = lpad 8 '0' $ showBin $ fn f1
    h2 = lpad 8 '0' $ showBin $ fn f2

showBin n = showIntAtBase 2 intToDigit n ""

lpad m y xs = replicate (m - length ys) y ++ ys
    where ys = take m xs

diff xs ys = 1.0 - (fromIntegral same) / fromIntegral (length xs) 
  where same = length $ filter (== True) $ zipWith (==) xs ys

modifyAt n f (x:xs)
  | n == 0 = (f x):xs
  | otherwise = x:modifyAt (n-1) f xs

-- Hash Functions

hashes :: Map.Map String HashFun
hashes = Map.fromList
  [ ("djb2", hash_djb2)
  , ("sdbm", hash_sdbm)
  , ("3", hash_3)
  , ("adler", hash_adler)
  , ("sum", sum)        
  , ("xor", foldl xor 0xff) ]       

hash_djb2 :: HashFun
hash_djb2 = foldl (\c h -> ((h `shiftL` 5) + h) + c) 5381

hash_sdbm :: HashFun
hash_sdbm = foldl (\c h -> c + (h `shiftL` 6) + (h `shiftL` 16)) 0

hash_3 :: HashFun
hash_3 = foldl (\c h -> (37*h + c) .&. 0xff) 0

hash_xor :: HashFun
hash_xor = foldl xor 0xff

hash_adler :: HashFun
hash_adler xs = (b `shiftL` 4) + a
  where
    (a, b) = foldr fn (1,0) xs
    fn c (s1, s2) = (s1', (s2 + s1') `mod` 13)
      where s1' = (s1 + c) `mod` 13
