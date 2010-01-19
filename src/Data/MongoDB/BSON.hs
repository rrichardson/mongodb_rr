module Data.MongoDB.BSON where

import Prelude hiding (length, readFile)
import Data.DateTime
import Data.ByteString hiding (zip, putStrLn, filter)
import qualified Data.ByteString.Char8 as C8
import Data.Binary.Put
import Data.Binary.Get
import Data.Word
import Data.Int
import Control.Monad
import Foreign.C.Types
import Unsafe.Coerce
import qualified Data.ByteString.Lazy as L
import Debug.Trace

data BSONValue = BDouble Double 
               | BString ByteString
               | BObject [(ByteString, BSONValue)]
               | BArray [BSONValue]
               | BBinary Int ByteString
               | BUndefined
               | BOid ByteString
               | BBool Bool
               | BDate DateTime
               | BNull
               | BEoo
               | BRegEx ByteString ByteString
               | BRef ByteString ByteString
               | BCode ByteString
               | BReserved2
               | BScopedCode ByteString
               | BInt Int
               | BLong Int
               | BTimestamp Int
               | BSymbol ByteString
               | BCodeScoped Int ByteString [(ByteString, BSONValue)] 
               deriving(Eq, Show, Ord)

type BSONError = String


bson_eoo = 0 :: Word8
bson_number_dbl = 1 :: Word8
bson_string = 2 :: Word8
bson_object = 3 :: Word8
bson_array = 4 :: Word8
bson_binary = 5 :: Word8
bson_undefined = 6 :: Word8
bson_oid = 7 :: Word8
bson_bool = 8 :: Word8
bson_date = 9 :: Word8
bson_null = 10 :: Word8
bson_regex = 11 :: Word8
bson_ref = 12 :: Word8
bson_code = 13 :: Word8
bson_symbol = 14 :: Word8
bson_code_scoped = 15 :: Word8
bson_number_int = 16 :: Word8
bson_timestamp = 17 :: Word8
bson_number_long = 18 :: Word8

-- 
-- PUT
-- 

putBSONValue :: BSONValue -> Put
putBSONValue (BString s) = putCStringSz s

putBSONValue (BSymbol s) = putCStringSz s 
 
putBSONValue (BBool b) = putWord8 $ boolToWord8 b

putBSONValue (BDate d) = putWord64le $ fromIntegral $ toSeconds d

putBSONValue (BInt i) = putWord32le $ fromIntegral $ i

putBSONValue (BLong i) = putWord64le $ fromIntegral $ i

putBSONValue (BTimestamp i) = putWord64le $ fromIntegral $ i

putBSONValue (BDouble d) = putWord64le $ unsafeCoerce ((realToFrac d) :: CDouble)

putBSONValue (BCode c) = putCStringSz c
 
putBSONValue (BCodeScoped tl s o) = do 
    putWord32le $ fromIntegral tl
    putCStringSz s
    putBSONObject o

putBSONValue (BRegEx r s) = do putByteString r
                               putWord8 0
                               putByteString s
                               putWord8 0

putBSONValue (BNull) = do return ()

putBSONValue (BUndefined) = do return ()

putBSONValue (BRef n s) = do putCStringSz n
                             putByteString s 

putBSONValue (BOid s) = putByteString s

putBSONValue (BBinary i s) = do putWord32le (fromIntegral (length s)) 
                                putWord8 (fromIntegral i)
                                putByteString s

putBSONValue (BObject o) = putBSONObject o

putBSONValue (BArray a) = do let c = zip [ C8.pack (show x) | x <- [0..]] a in
                                 putBSONValue $ BObject c

putBSONObject o = let str = (runPut $ putBSONValue' o) in do
                                putWord32le $ fromIntegral $ (L.length str) + 4
                                putByteString $ (C8.concat . L.toChunks) str

putBSONValue' o = do mapM_ putBSONElement o
                     putWord8 bson_eoo

putBSONElement (k, v) = do putElementType v
                           putCString k
                           putBSONValue v

getObjectSize o = 0

putElementType :: BSONValue -> Put
putElementType (BDouble _) =   putWord8 bson_number_dbl
putElementType (BString _) =   putWord8 bson_string
putElementType (BBool _) =     putWord8 bson_bool
putElementType (BDate _) =     putWord8 bson_date
putElementType (BInt _) =      putWord8 bson_number_int
putElementType (BLong _) =     putWord8 bson_number_long
putElementType (BCode _) =     putWord8 bson_code
putElementType (BRegEx _ _) =  putWord8 bson_regex
putElementType (BNull) =       putWord8 bson_null
putElementType (BOid _) =      putWord8 bson_oid
putElementType (BBinary _ _) = putWord8 bson_binary
putElementType (BRef _ _) =    putWord8 bson_ref
putElementType (BArray _) =    putWord8 bson_array
putElementType (BObject _ ) =  putWord8 bson_object
putElementType (BSymbol _) =   putWord8 bson_symbol
putElementType (BCodeScoped _ _ _) =  putWord8 bson_code_scoped
putElementType (BUndefined) =  putWord8 bson_undefined
--
-- GET
-- 

getBSONValue :: Word8 -> Get BSONValue
getBSONValue 1 = (getWord64le >>= \d -> return $ BDouble $ realToFrac $ (unsafeCoerce :: a -> CDouble) d) 
getBSONValue 2 = do l <- getWord32le
                    s <- getByteString $ (fromIntegral l) - 1
                    _ <- getWord8
                    return $ BString s
getBSONValue 3 = getBSONObject
getBSONValue 4 = getBSONArray
getBSONValue 5 = do l <- getWord32le
                    t <- getWord8
                    s <- getByteString $ (fromIntegral l)
                    return $ BBinary (fromIntegral t) s
getBSONValue 6 = return BUndefined 
getBSONValue 7 = liftM BOid (getByteString 12)
getBSONValue 8 = (getWord8 >>= \b -> if b==1 then return $ BBool True else return $ BBool False)
getBSONValue 9 = (getWord64le >>= \i -> return $ BDate $ fromSeconds $ fromIntegral i)
getBSONValue 10 = return BNull 
getBSONValue 11 = do a <- getCString
                     b <- getCString
                     return $ BRegEx a b
getBSONValue 12 = do l <- getWord32le
                     n <- getByteString $ (fromIntegral l) - 1 
                     _ <- getWord8
                     s <- getByteString 12
                     return (BRef n s)
getBSONValue 13 = do (BString s) <- getBSONValue 2
                     return (BCode s)
getBSONValue 14 = do (BString s) <- getBSONValue 2
                     return (BSymbol s)
getBSONValue 15 = do tl <- getWord32le
                     (BString s) <- getBSONValue 2
                     (BObject o) <- getBSONObject
                     return $ BCodeScoped (fromIntegral tl) s o
getBSONValue 16 = getWord32le >>= \i -> return $ BInt $ fromIntegral i
getBSONValue 17 = getWord64le >>= \i -> return $ BLong $ fromIntegral i
getBSONValue 18 = getWord64le >>= \i -> return $ BTimestamp $ fromIntegral i

getBSONValue x = error $ "unknown BSON element id: " ++ (show x)

getBSONElement = do t <- getWord8
                    if t == 0 
                        then return (C8.empty, BEoo)
                        else do
                            n <- getCString
                            v <- getBSONValue t
                            return (n, v)

getBSONObject :: Get BSONValue 
getBSONObject = getWord32le >> getElements []
                where getElements ls = do 
                      e <-getBSONElement
                      case e of
                          (_, BEoo) -> return $ BObject $ ls
                          otherwise -> getElements $ ls ++ [e]

getBSONArray :: Get BSONValue 
getBSONArray = getBSONObject >>= \(BObject o) -> return $ BArray [ snd x | x <- o ]

-- Item extraction from BSONValue

findItem :: ByteString -> BSONValue -> Maybe BSONValue
findItem key (BObject barray)  = findItem' $ filter (\t -> fst t == key) barray
        where findItem' [] = Nothing 
              findItem' (x:xs) = Just (snd x) 

findItem key notobj = Nothing

toStr :: BSONValue -> Maybe ByteString
toStr (BString str) = Just str
toStr x = Nothing

toStr' :: BSONValue -> ByteString
toStr' (BString str) = str
toStr' (BOid str) = str
toStr' other = empty

toInt :: BSONValue -> Maybe Int
toInt (BInt i) = Just i
toInt other = Nothing

--
-- Utilities
--

getCString :: Get ByteString
getCString = build empty
             where build s = (getWord8 >>= \c -> if c == 0 then return s else build (snoc s c))

putCStringSz s = do putWord32le $ fromIntegral $ (length s) + 1
		    putCString s 

putCString s = do putByteString s
                  putWord8 0

boolToWord8 :: Bool -> Word8 
boolToWord8 = fromIntegral . fromEnum

