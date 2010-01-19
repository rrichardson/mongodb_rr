module Data.MongoDB.MongoClient
  ( 
      MongoContext
     ,launch
     ,query
     ,save
     ,update
     ,remove
     ,findOne
     ,limit
     ,sort
     ,getMore
     ,cleanup
     ,msg
     ,quit
     ,runCommand
     --,login
     ,debugResponse
  )
where

import Prelude hiding (catch)
import Network (connectTo, withSocketsDo, PortID(..), HostName)
import Network.Socket (sIsConnected)
import System.IO
import System.IO.Error (isEOFError)
import System.Environment (getArgs)
import Control.Exception (finally, catch, Exception(..))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (liftM, ap)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as B
import Data.Digest.OpenSSL.MD5
import Data.Binary.Put
import Data.Binary.Get
import Data.MongoDB.MongoWire
import Data.MongoDB.BSON (BSONValue(..))

type Port = Int
type RequestId = Int
type RequestMap = M.Map RequestId (Either MDbCallback (TChan MongoResponse))
bstr = C8.pack 

hdr_size = 16

data MContext = MContext { cmdChan    :: TChan (MongoRequest, RequestId)
                          ,ctxSocket  :: Handle
                          ,reqMap     :: RequestMap 
                          ,reqCounter :: Int }

newtype MongoContext = Ctx (TMVar MContext)

{- Public Functions -}

debugResponse :: MongoResponse -> IO()
debugResponse = putStrLn . show
 
save :: MongoContext -> B.ByteString -> [BSONValue] -> Maybe MDbCallback -> IO (MongoResponse)
save ctx collection value cb = 
    let req = MDbInsert collection value
    in sendRequest ctx req cb

query :: MongoContext -> B.ByteString -> Int -> Int -> BSONValue -> BSONValue -> Maybe MDbCallback -> IO(MongoResponse)
query ctx collection skip ret value selector cb = 
    let req = MDbQuery collection skip ret value selector
    in sendRequest ctx req cb

update :: MongoContext -> B.ByteString -> Int -> BSONValue -> BSONValue -> Maybe MDbCallback -> IO(MongoResponse)
update ctx collection flags selector doc cb = 
    let req = MDbUpdate collection flags selector doc
    in sendRequest ctx req cb

remove :: MongoContext -> B.ByteString -> BSONValue -> Maybe MDbCallback -> IO(MongoResponse)
remove ctx collection selector cb = 
    let req = MDbDelete collection selector
    in sendRequest ctx req cb

findOne :: MongoContext -> B.ByteString -> BSONValue -> BSONValue -> Maybe MDbCallback -> IO(MongoResponse)
findOne ctx collection value selector cb = query ctx collection 0 1 value selector cb

runCommand :: MongoContext -> BSONValue -> IO(MongoResponse)
runCommand ctx val = findOne ctx (bstr "admin.$cmd") val BNull Nothing

{-
login :: MongoContext -> B.ByteString -> B.ByteString -> Bool
login ctx user pass = do
    result <- runCommand ctx $ BObject [( (bstr "getnonce"), BInt (1) )]
    case findItem (bstr "nonce") $ (head . rplDocuments) result of
        Nothing -> return False
        Just nonce -> do 
            let digest = md5 [toStr' nonce, user, md5 [user, bstr ":mongo:", pass]]
            res <- runCommand ctx $ BObject [ (bstr "authenticate", BInt 1)
                                             ,(bstr "user", BString user)
                                             ,(bstr "nonce", nonce)
                                             ,(bstr "key", digest )]
            findItem (bstr "ok") -- use next operations in maybe monad findItem / getInt
-}             


addUser :: MongoContext -> B.ByteString -> B.ByteString -> IO (MongoResponse)
addUser ctx user pass = undefined

limit = undefined

sort = undefined

getMore = undefined

cleanup = undefined

msg = undefined

wait = undefined 

quit = undefined


launch :: HostName -> Port -> IO (MongoContext)
launch host port = withSocketsDo $ do 
    sock <- connectTo host $ PortNumber $ fromIntegral port
    hSetBuffering sock NoBuffering 
    ctx <- atomically (newContext sock)
    spawn $ listenLoop (hGetReply sock >>= handleResponse ctx)
    spawn $ cmdLoop ctx
    return ctx

{- Private Functions -}

newContext :: Handle -> STM MongoContext
newContext sock = do context <- MContext `liftM` newTChan `ap` return sock `ap` return M.empty `ap` return 0
                     Ctx `liftM` newTMVar context

hGetReply :: Handle -> IO (MongoHeader, MongoResponse)  
hGetReply sock = do
    putStrLn "in hGetReply"
    hdr <- liftM (runGet getHeader) $ L.hGet sock hdr_size
    putStrLn $ "got header" ++ (show hdr)
    resp <- liftM (runGet getReply) $ L.hGet sock ((messageLength hdr) - hdr_size)
    putStrLn $ "received response: " ++  (show resp)
    return (hdr, resp)

spawn :: IO () -> IO ThreadId
spawn act = do
    mainTID <- myThreadId
    forkIO $ act `catch` throwTo mainTID -- \e -> throwTo mainTID  (e :: SomeException)

listenLoop :: IO () -> IO ()
listenLoop = sequence_  . repeat

cmdLoop :: MongoContext -> IO ()
cmdLoop (Ctx ctx) = do
    putStrLn "in cmdLoop"
    mctx <- atomically $ readTMVar ctx
    (command, rid) <- atomically $ readTChan (cmdChan mctx)
    putStrLn $ "received command" ++ (show command)
    hFlush stdout
    case command of 
      MQuit -> shutdown (Ctx ctx)
      otherwise -> do
          let str = runPut $ putRequest command rid 0 
          L.putStrLn str
          B.hPutStr (ctxSocket mctx) ((C8.concat . L.toChunks) str)
          hFlush (ctxSocket mctx)
          cmdLoop (Ctx ctx)  

shutdown :: MongoContext -> IO ()
shutdown (Ctx ctx) = undefined

-- build response object, check map to see if it is responding to a request that is in the map
-- if so, call the associated callback and return Nothing
-- otherwise, return the MongoResponse to the listenLoop to be sent to the user
handleResponse :: MongoContext -> (MongoHeader, MongoResponse) -> IO ()
handleResponse ctx (hdr, resp) = do
    putStrLn $ (show hdr) ++ " - " ++ (show resp)
    result <- lookupMap ctx (responseTo hdr)
    case result of 
        Nothing -> return () 
        Just mresp -> do 
            removeFromMap ctx (responseTo hdr)
            case mresp of
                Left cb -> forkIO (cb resp) >> return () 
                Right chan -> atomically $ writeTChan chan resp 

sendRequest :: MongoContext -> MongoRequest -> Maybe MDbCallback -> IO (MongoResponse)
sendRequest ctx req Nothing = do
    newchan <- atomically newTChan
    mctx' <- withContext ctx (\mctx -> return mctx { reqMap = (M.insert (reqCounter mctx +1)  (Right newchan) (reqMap mctx)) , 
                                                     reqCounter = reqCounter mctx + 1 })
    atomically $ writeTChan (cmdChan mctx') (req, reqCounter mctx')
    hFlush stdout
    atomically $ readTChan newchan

sendRequest ctx req (Just cb) = do
    mctx' <- withContext ctx (\mctx ->
        let c = updCtx mctx cb
        in atomically $ writeTChan (cmdChan mctx) (req, (reqCounter mctx)) >> return c)
    return nullResponse 

nullResponse = MongoResponse 0 0 0 0 [BNull]
 
updCtx mctx cb = mctx { reqMap = (M.insert (reqCounter mctx + 1) (Left cb) (reqMap mctx)) , 
                     reqCounter = (reqCounter mctx + 1) }

newRequestId :: MongoContext -> IO (RequestId)
newRequestId ctx = do 
    mctx' <- withContext ctx (\mctx -> return mctx { reqCounter = reqCounter mctx + 1 })
    return (reqCounter mctx')

lookupMap :: MongoContext -> RequestId -> IO (Maybe (Either MDbCallback (TChan MongoResponse)))
lookupMap (Ctx ctx) id = do 
    mctx <- atomically $ takeTMVar ctx 
    let result = M.lookup id $ reqMap mctx
    atomically $ putTMVar ctx mctx
    return result

removeFromMap :: MongoContext -> RequestId -> IO ()
removeFromMap (Ctx ctx) id = do 
    mctx <- atomically $ takeTMVar ctx 
    let result = M.delete id $ reqMap mctx
    atomically $ putTMVar ctx mctx

withContext :: MongoContext -> (MContext -> IO(MContext)) -> IO(MContext)
withContext (Ctx var) act = do 
    inner <- atomically $ takeTMVar var
    inner'<- act inner 
    atomically $ putTMVar var inner'
    return inner'

md5 :: [B.ByteString] -> B.ByteString
md5 barray = C8.pack $ md5sum (B.concat barray)
