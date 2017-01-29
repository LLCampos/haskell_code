module Main where
import System
import Data.Array

fact n = product [1..n]

fetch _ []    = error "String too short to support such a permutation."
fetch 0 (h:t) = (h,t)
fetch n (h:t) = (e, h:s)
    where (e,s) = fetch (n-1) t

perm _ [] = []
perm n list = e : perm (n `mod` s) l
    where s = fact . subtract 1 . length $ list
          (e,l) = fetch (div n s) list

choice n s | n > 0  = perm (n-1) s
           | n < 0  = show . map (flip perm s) . take (negate n) $ [0..]
           | n == 0 = "prog n s\n\
                      \  n > 0 --> nth permutation of s\n\
                      \  n < 0 --> first (-n) permutations of s\n"

main = do [n,s] <- getArgs
          putStrLn $ choice (read n) s