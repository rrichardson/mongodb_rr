module BsonTest where
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as I
import Data.ByteString hiding (putStrLn,map,zip, filter)
import System.Directory 
import Control.Monad 
import Debug.Trace
import BSONValues

data TestResult = TestResult { filename :: String, bytes :: L.ByteString,  obj :: BSONValue }

main = mapM loadDir ["basic_types", "complex", "single_types"]

loadDir n = do paths <- getDirectoryContents n
               let files = filter (isSuffixOf (C8.pack "bson") . C8.pack) paths 
               putStrLn (show files)
               globs <- sequence [ L.readFile $ mkFile n f | f <- files ]
               let results = [ TestResult (mkFile n (fst fb)) (snd fb) (runObjects (snd fb)) | fb <- zip files globs]
               mapM testPut results 

runObjects = runGet getBSONObject 

mkFile d f = d++"/"++f
 
putObj :: (FilePath, BSONValue) -> IO ()
putObj (f, o) = let str = runPut $ putBSONValue o in L.writeFile (f ++ "2") str

testPut :: TestResult -> IO ()
testPut (TestResult f b o) = let str = (runPut $ putBSONValue o) in 
				 if str == b then putStrLn $ f ++ ":: SUCCEEDED"
                                             else do putStrLn $ f ++ ":: FAILED" 
                                                     L.writeFile (f ++ ".failed") str 

--rightseq = sequence . map (uncurry (fmap fmap (,)))
--rightseq (bs, io) = do { m <- io; return (bs, m) }
