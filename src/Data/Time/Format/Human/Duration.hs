module Data.Time.Format.Human.Duration
    ( Tense(..)
    , Unit(..)
    , Duration(..)
    , toDuration
    ) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

data Tense = Past deriving (Eq, Show)

data Unit = Seconds | Minutes deriving (Eq, Show)

data Duration = Duration
    { durationTense :: !Tense
    , durationUnit :: !Unit
    , durationValue :: !Int
    } deriving (Eq, Show)

toDuration :: UTCTime -> UTCTime -> Duration
toDuration now = helper . diffUTCTime now
  where
    helper :: NominalDiffTime -> Duration
    helper d
        | d > -1 && d < 1 = Duration Past Seconds 0
        | d > -60 && d < 60 = Duration Past Seconds $ toSeconds d
        | d > -(60*60) && d < (60*60) = Duration Past Minutes $ toMinutes d
        | otherwise = undefined

    toSeconds :: NominalDiffTime -> Int
    toSeconds = truncate

    toMinutes :: NominalDiffTime -> Int
    toMinutes s = toSeconds $ s / 60
