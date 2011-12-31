{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Instances ( TwitterTime(..), lookup'
                 , lookupDeep, fromJSON', insertJS) where
import Data.Traversable
import Prelude hiding (mapM, lookup)
import Data.Aeson
import qualified Data.Bson as Bson
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import qualified Data.CompactString as US
import Data.Attoparsec.Number
import System.Locale
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Arrow
import Data.Maybe

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

findWithDefault :: FromJSON a => a -> T.Text -> Value -> a
findWithDefault = (. lookup') . (.) . fromMaybe

{-# SPECIALIZE lookup' :: T.Text -> Value -> Maybe Value #-}
lookup' :: FromJSON a => T.Text -> Value -> Maybe a
lookup' key (Object dic) = fromJSON' =<< M.lookup key dic
lookup' _ _              = Nothing

{-# SPECIALIZE lookupDeep :: [T.Text] -> Value -> Maybe Value #-}
lookupDeep :: FromJSON a => [T.Text] -> Value -> Maybe a
lookupDeep = ((fromJSON' =<<) .) . foldr (>=>) return . map lookup'

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

insertJS :: ToJSON a => T.Text -> a -> Value -> Value
insertJS k v (Object ob) = Object $ M.insert k (toJSON v) ob
insertJS _ _ v           = v

instance ToJSON Bson.Binary where
  toJSON (Bson.Binary bs) = toJSON bs

instance ToJSON Bson.Function where
  toJSON (Bson.Function fn) = toJSON fn

instance ToJSON Bson.UString where
  toJSON = toJSON . US.unpack

instance ToJSON Bson.UUID where
  toJSON (Bson.UUID uuid) = toJSON uuid

instance ToJSON Bson.MD5 where
  toJSON (Bson.MD5 uuid) = toJSON uuid

instance ToJSON Bson.UserDefined where
  toJSON (Bson.UserDefined uuid) = toJSON uuid

instance ToJSON Bson.ObjectId where
  toJSON (Bson.Oid w32 w64) = toJSON $ toInteger w32 * 2^64 + toInteger w64

instance ToJSON Bson.Regex where
  toJSON (Bson.Regex us1 us2) = toJSON $ us1 `US.append` us2

instance ToJSON Bson.Javascript where
  toJSON (Bson.Javascript _ us) = toJSON us

instance ToJSON Bson.Symbol where
  toJSON (Bson.Symbol us) = toJSON us

instance ToJSON Bson.MongoStamp where
  toJSON (Bson.MongoStamp i) = toJSON i

instance ToJSON Bson.MinMaxKey where
  toJSON Bson.MinKey = toJSON (I $ -1)
  toJSON Bson.MaxKey = toJSON (I 1)

instance ToJSON Bson.Value where
  toJSON (Bson.Float dbl) = toJSON dbl
  toJSON (Bson.String us) = toJSON $ US.unpack us
  toJSON (Bson.Doc doc)   = object $ map toPair doc
    where toPair (k Bson.:= v) = T.pack (US.unpack k) .= v
  toJSON (Bson.Array ar)  = Array $ V.fromList $ map toJSON ar
  toJSON (Bson.Bin bs)    = toJSON bs
  toJSON (Bson.Fun func)  = toJSON func
  toJSON (Bson.Uuid uui)  = toJSON uui
  toJSON (Bson.Md5 md5)   = toJSON md5
  toJSON (Bson.UserDef u) = toJSON u
  toJSON (Bson.ObjId oid) = toJSON oid
  toJSON (Bson.Bool bool) = toJSON bool
  toJSON (Bson.UTC utct)  = toJSON utct
  toJSON (Bson.Null)      = Null
  toJSON (Bson.RegEx reg) = toJSON reg
  toJSON (Bson.JavaScr j) = toJSON j
  toJSON (Bson.Sym sym)   = toJSON sym
  toJSON (Bson.Int32 i32) = toJSON i32
  toJSON (Bson.Int64 i64) = toJSON i64
  toJSON (Bson.Stamp stm) = toJSON stm
  toJSON (Bson.MinMax mm) = toJSON mm

instance Data.Aeson.FromJSON [Bson.Field] where
  parseJSON (Object v) = map (uncurry (Bson.:=) . first (US.pack . T.unpack)) . M.toList <$> mapM parseJSON v
  parseJSON _ = mzero

instance FromJSON Bson.Value where
  parseJSON (Object v) = Bson.Doc . map (uncurry (Bson.:=) . first (US.pack . T.unpack)) . M.toList <$> mapM parseJSON v
  parseJSON (Array ar) = Bson.Array <$> mapM parseJSON (V.toList ar)
  parseJSON (String s) = pure $ Bson.String $ US.pack $ T.unpack s
  parseJSON (Number (I i)) | floor (logBase 2 $ fromIntegral i) <= 32 = pure $ Bson.Int32 $ fromIntegral i
                           | otherwise       = pure $ Bson.Int64 $ fromIntegral i
  parseJSON (Number (D d))                   = pure $ Bson.Float d
  parseJSON (Bool b)   = pure $ Bson.Bool b
  parseJSON (Null)     = pure Bson.Null

newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }
    deriving (Eq, Ord)

instance Show TwitterTime where
  show = formatTime defaultTimeLocale "%c" . fromTwitterTime

instance Read TwitterTime where
  readsPrec _ = Prelude.map (first TwitterTime) . readsTime defaultTimeLocale "%c"

instance FromJSON TwitterTime where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%c" (T.unpack t) of
      Just t  -> return $ TwitterTime t
      Nothing -> fail "parsing twitter time format fail"
  parseJSON _          = mzero
