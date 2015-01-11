module Data.Time.Format.Human.Duration
    ( Tense(..)
    , Unit(..)
    , Duration(..)
    , toDuration
    ) where

import Data.Time (UTCTime)

data Tense = Past deriving (Eq, Show)

data Unit = Seconds deriving (Eq, Show)

data Duration = Duration
    { durationTense :: !Tense
    , durationUnit :: !Unit
    , durationValue :: !Int
    } deriving (Eq, Show)

toDuration :: UTCTime -> UTCTime -> Duration
toDuration _ _ = Duration Past Seconds 0
