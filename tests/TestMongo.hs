module Main where


import Data.MongoDB.MongoClient
import Data.MongoDB.BSON
import qualified Data.ByteString.Char8 as C8

testAsync ctx = do 
    --res <- runCommand ctx $ BObject [ (C8.pack "nonce", BInt 1) ] 
    res <- findOne ctx (C8.pack "test.people") (BObject [ (C8.pack "num", BInt 15)] ) BNull (Just debugResponse)
    putStrLn (show res)

testSync ctx = do 
    res <- runCommand ctx $ BObject [ (C8.pack "getnonce", BInt 1) ] 
    -- res <- findOne ctx (C8.pack "test.people") (BObject [ (C8.pack "num", BInt 15)] ) BNull Nothing 
    putStrLn (show res)
