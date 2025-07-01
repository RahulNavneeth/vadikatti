module Main where

import Bow
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

type P = Float

pSpam :: P
pSpam = 0.3

pHam :: P
pHam = 1.0 - pSpam

nbc :: Bow -> Bow -> T.Text -> P
nbc (Bow hbow) (Bow sbow) word = 
    (pDSpam * pSpam)/((pDSpam * pSpam) + (pDHam * pHam))
    where
        mapBow bow w = (fromMaybe 0 (M.lookup (T.pack w) bow))	
        mapPD bow freq = (fromIntegral freq)/(fromIntegral (length (M.toList bow)))

        wordSFreq = map (mapBow sbow) (words $ T.unpack word)
        pDSpam = foldl (*) 1 (map (mapPD sbow) wordSFreq)

        wordHFreq = map (mapBow hbow) (words $ T.unpack word)
        pDHam = foldl (*) 1 (map (mapPD hbow) wordHFreq)

readBow :: FilePath -> IO Bow
readBow path = do
    content <- readFile (dataPath ++ path)
    let bow = map (parse . words) (lines content)
    return (mconcat bow)
  where
    parse (x:y:_) = Bow $ M.fromList [(T.pack x, read y :: Int)]

main :: IO ()
main = do
    bowHam <- readBow "/ham-frequency.txt"
    bowSpam <- readBow "/spam-frequency.txt"
    print "Enter input: "
    input <- getLine
    if nbc bowHam bowSpam (T.pack (head $ words input)) <= pSpam
    then print "HAM"
    else print "SPAM"
