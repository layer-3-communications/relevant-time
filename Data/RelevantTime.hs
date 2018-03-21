{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

module Data.RelevantTime
  ( RelevantTime(..)
  , encodeRelevantTime
  , decodeRelevantTime
  , absolutizeRelevantTime
  ) where

import Data.Text (Text)
import Data.Aeson (ToJSON(..),FromJSON(..))
import Data.Int (Int64)
import Chronos.Types (Time)
import Torsor (add,invert,scale)
import qualified Chronos as CH
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Aeson as AE

data RelevantTime
  = RelevantTimeMinute {-# UNPACK #-} !Int64
  | RelevantTimeHour   {-# UNPACK #-} !Int64
  | RelevantTimeDay    {-# UNPACK #-} !Int64
  | RelevantTimeNow
  | RelevantTimeMidnight
  | RelevantTimeNoon
  deriving (Eq,Show)

instance ToJSON RelevantTime where
  toJSON = AE.String . encodeRelevantTime

instance FromJSON RelevantTime where
  parseJSON = AE.withText "RelevantTime"
    (maybe (fail "invalid RelevantTime") return . decodeRelevantTime)

encodeRelevantTime :: RelevantTime -> Text
encodeRelevantTime = \case
  RelevantTimeNow -> "now"
  RelevantTimeMidnight -> "midnight"
  RelevantTimeNoon -> "noon"
  RelevantTimeMinute x -> T.pack (show x ++ "m")
  RelevantTimeHour x -> T.pack (show x ++ "h")
  RelevantTimeDay x -> T.pack (show x ++ "d")

decodeRelevantTime :: Text -> Maybe RelevantTime
decodeRelevantTime t = if T.null t
  then Nothing
  else case t of
    "now" -> Just RelevantTimeNow
    "noon" -> Just RelevantTimeNoon
    "midnight" -> Just RelevantTimeMidnight
    _ -> case T.last t of
      'm' -> RelevantTimeMinute <$> readInt (T.init t)
      'h' -> RelevantTimeHour <$> readInt (T.init t)
      'd' -> RelevantTimeDay <$> readInt (T.init t)
      _ -> Nothing

absolutizeRelevantTime :: Time -> RelevantTime -> Time
absolutizeRelevantTime now x = case x of
  RelevantTimeNow -> now
  RelevantTimeNoon-> add (scale 12 CH.hour) (CH.dayToTimeMidnight (CH.timeToDayTruncate now))
  RelevantTimeMidnight -> CH.dayToTimeMidnight (CH.timeToDayTruncate now)
  RelevantTimeMinute n -> add (invert (scale n CH.minute)) now
  RelevantTimeHour n -> add (invert (scale n CH.hour)) now
  RelevantTimeDay n -> add (invert (scale n CH.day)) now

readInt :: Text -> Maybe Int64
readInt x = case TR.signed TR.decimal x of
  Left _ -> Nothing
  Right (i,leftover) -> if T.null leftover
    then Just i
    else Nothing
