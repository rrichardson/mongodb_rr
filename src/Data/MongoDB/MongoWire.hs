module MongoWire
where

import Prelude hiding (length, readFile)
import Data.DateTime
import Data.ByteString hiding (zip, putStrLn)
import qualified Data.ByteString.Char8 as C8
import Data.Binary.Put
import Data.Binary.Get
import Data.Int
import Control.Monad
import Foreign.C.Types
import Unsafe.Coerce
import qualified Data.ByteString.Lazy as L
import Debug.Trace
import BSONValues


data MongoRequest = MDbUpdate { updCollectionName :: ByteString
                               ,updFlags          :: Int 
                               ,updSelector       :: BSONValue
                               ,updDocument       :: BSONValue }
                  | MDbInsert { insCollectionName :: ByteString
                               ,insDocuments      :: [BSONValue] }
                  | MDbQuery { qryCollectionName :: ByteString 
                              ,qryNumberToSkip   :: Int
                              ,qryNumberToReturn :: Int
                              ,qryQuery          :: BSONValue
                              ,qryFieldSelector  :: BSONValue }
                  | MDbGetMore { getCollectionName :: ByteString
                                ,getNumberToReturn :: Int
                                ,getCursorID       :: Int }
                  | MDbDelete { delCollectionName :: ByteString
                               ,delSelector       :: BSONValue }
                  | MDbKillCursors { cursorIds :: [Int] } 
                  | MDbMsg { message :: ByteString } 
                  | MQuit 
                    deriving (Show)

type MDbCallback = MongoResponse -> IO()

data MongoHeader = MongoHeader { messageLength :: Int
                                ,requestId     :: Int
                                ,responseTo    :: Int
                                ,opCode        :: Int }
                                deriving (Show)

data MongoResponse = MongoResponse { responseFlag   :: Int
                                    ,cursorId       :: Int64 
                                    ,startingFrom   :: Int
                                    ,numberReturned :: Int
                                    ,rplDocuments      :: [BSONValue] }
                                     deriving (Show)

getOpcode :: MongoRequest -> Int
getOpcode (MDbUpdate _ _ _ _) = 2001
getOpcode (MDbInsert _ _) = 2002
getOpcode (MDbQuery _ _ _ _ _ ) = 2004
getOpcode (MDbGetMore _ _ _) = 2005
getOpcode (MDbDelete _ _ ) = 2006
getOpcode (MDbKillCursors _) = 2007
getOpcode (MDbMsg _) = 1000
-- no op_get_by_oid = 2003

          
putRequest :: MongoRequest -> Int -> Int -> Put
putRequest req rid rto = do
    let s = runPut $ putBody req
    putHeader $ MongoHeader (fromIntegral (L.length s) + 16) rid rto (getOpcode req)
    putByteString $ (C8.concat . L.toChunks) s

putBody :: MongoRequest -> Put
putBody (MDbUpdate q f s d) = do 
    putCStringSz q
    putWord32le $ fromIntegral f 
    putBSONValue s
    putBSONValue d 

putBody (MDbInsert c d) = putCStringSz c >> mapM_ putBSONValue d
        
putBody (MDbQuery c ns nr q fs) = do 
    putCStringSz c 
    putWord32le $ fromIntegral ns 
    putWord32le $ fromIntegral nr 
    putBSONValue q
    putBSONValue fs

putBody (MDbGetMore c nr cid) = do
    putCStringSz c 
    putWord32le $ fromIntegral nr 
    putWord32le $ fromIntegral cid 

putBody (MDbDelete c s) = do
    putCStringSz c 
    putBSONValue s 

putBody (MDbKillCursors ids) = mapM_ (putWord32le . fromIntegral) ids

putBody (MDbMsg m) = putCStringSz m 

putHeader :: MongoHeader -> Put
putHeader (MongoHeader l id to op) = do 
    putWord32le $ fromIntegral l  
    putWord32le $ fromIntegral id
    putWord32le $ fromIntegral to
    putWord32le $ fromIntegral op

getReply :: Get MongoResponse
getReply = do r <- getIntFromW32 
              c <- getIntFromW64
              s <- getIntFromW32  
              n <- getIntFromW32
              d <- getReplyObjects n []
              return $ MongoResponse r c s n d

getReplyObjects :: Int -> [BSONValue] -> Get [BSONValue]
getReplyObjects num objs = do 
    if num == 0 then return objs
        else getBSONObject >>= \o -> getReplyObjects (num - 1) (o:objs)

getHeader :: Get MongoHeader
getHeader = do l <- getIntFromW32
               i <- getIntFromW32
               t <- getIntFromW32
               o <- getIntFromW32
               return $ MongoHeader l i t o

getIntFromW32 :: Get Int
getIntFromW32 = getWord32le >>= return . fromIntegral

getIntFromW64 :: Get Int64
getIntFromW64 = getWord64le >>= return . fromIntegral

