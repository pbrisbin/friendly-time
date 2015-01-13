module Data.Time.Format.Human.Duration
    ( Tense(..)
    , Unit(..)
    , Duration(..)
    , toDuration
    ) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

data Tense = Past | Future deriving (Eq, Show)

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
        | between d (-1) 1 = Duration Past Seconds 0
        | between d 0 60 = Duration Past Seconds $ toSeconds d
        | between d (-60) 0 = Duration Future Seconds $ negate $ toSeconds d
        | between d 0 3600 = Duration Past Minutes $ toMinutes d
        | between d (-3600) 0 = Duration Future Minutes $ negate $ toMinutes d
        | otherwise = undefined

    toSeconds :: NominalDiffTime -> Int
    toSeconds = truncate

    toMinutes :: NominalDiffTime -> Int
    toMinutes s = toSeconds $ s / 60

    between :: Ord a => a -> a -> a -> Bool
    between d m n = d > m && d < n
