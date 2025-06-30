module Frequency (frequency) where

import System.Directory
import Control.Monad (when)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import Data.Semigroup
import Data.Monoid
import Data.Foldable
import Data.Function
import Data.List
import Text.Printf
import System.IO.Unsafe (unsafePerformIO)

data Bow = Bow { bowToMap :: M.Map T.Text Int }

instance Semigroup Bow where
	Bow a <> Bow b = Bow $ M.unionWith (+) a b

instance Monoid Bow where
	mempty = Bow M.empty

instance Show Bow where
  show (Bow a) =
    unlines [printf "%s %d" (T.unpack word) freq | (word, freq) <- sortBy (compare `on` snd) (M.toList a)]

dataPath :: FilePath
dataPath = "./data"

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
	files <- listDirectory dir
	return $ take 10 $ map (dir </>) files

readContent :: FilePath -> IO T.Text
readContent path = TIO.readFile path

textToBow :: IO T.Text -> IO [Bow]
textToBow text = text >>= \content -> return $ bow (T.unpack content)
	where
		bow c = map parseEntry (words c)
		parseEntry x = Bow $ M.fromList [((T.toLower . T.pack) x, 1)]

getFrequency :: FilePath -> IO Bow
getFrequency path = do
    hamFiles <- getFiles ((dataPath ++ path))
    bow <- mapM (textToBow . readContent) hamFiles
    return $ mconcat (concat bow)

getBowValue :: Bow -> [(T.Text, Int)]
getBowValue (Bow bow) = M.toList bow

writeToFile :: FilePath -> Bow -> IO ()
writeToFile path bow = 
	when (length content > 0) $
		writeFile (dataPath ++ path) content
	where
		content = (show bow)
 
frequency :: IO ()
frequency = do
    ham <- getFrequency "/enron1/ham/"
    spam <- getFrequency "/enron1/spam/"
    writeToFile "/ham-frequency.txt" ham
    writeToFile "/spam-frequency.txt" spam
