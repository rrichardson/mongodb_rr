module BSON where

import BsonValues.h

class BSON a where
    toBSONValue :: a -> BSONValue
    fromBSONValue :: BSONValue -> Either BSONError a

instance BSON BSONValue where
    toBSONValue = id
    fromBSONValue = Right

