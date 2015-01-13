module Data.Time.Format.Human.Duration
    ( Tense(..)
    , Unit(..)
    , Duration(..)
    , toDuration
    ) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

data Tense = Past | Future deriving (Eq, Show)

data Unit = Seconds | Minutes | Hours | Days deriving (Eq, Show)

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
        | between (seconds d) 0 60 = Duration Past Seconds $ seconds d
        | between (seconds d) (-60) 0 = Duration Future Seconds $ negate $ seconds d
        | between (minutes d) 0 60 = Duration Past Minutes $ minutes d
        | between (minutes d) (-60) 0 = Duration Future Minutes $ negate $ minutes d
        | between (hours d) 0 24 = Duration Past Hours $ hours d
        | between (hours d) (-24) 0 = Duration Future Hours $ negate $ hours d
        | between (days d) 0 10 = Duration Past Days $ days d
        | between (days d) (-10) 0 = Duration Future Days $ negate $ days d
        | otherwise = undefined

    seconds :: NominalDiffTime -> Int
    seconds = truncate

    minutes :: NominalDiffTime -> Int
    minutes s = truncate $ s / 60

    hours :: NominalDiffTime -> Int
    hours s = truncate $ s / 3600

    days :: NominalDiffTime -> Int
    days s = truncate $ s / 86400

    between :: Ord a => a -> a -> a -> Bool
    between d m n = d > m && d < n
