module Data.Time.Format.Human.Duration
    ( Tense(..)
    , Unit(..)
    , Duration(..)
    , durationValue
    , toDuration
    ) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

data Tense = Past | Future deriving (Eq, Show)

data Unit
    = Seconds
    | Minutes
    | Hours
    | Days
    | Weeks
    deriving (Eq, Show)

data Duration
    = Recent !Tense !Unit !Int
    | ThisWeek !UTCTime
    | ThisYear !UTCTime
    | Absolute !UTCTime
    deriving (Eq, Show)

durationValue :: Duration -> Maybe Int
durationValue (Recent _ _ x) = Just x
durationValue _ = Nothing

toDuration :: UTCTime -> UTCTime -> Duration
toDuration now t = helper $ diffUTCTime now t
  where
    helper :: NominalDiffTime -> Duration
    helper d
        | between d (-1) 1 = Recent Past Seconds 0
        | between (seconds d) 0 60 = Recent Past Seconds $ seconds d
        | between (seconds d) (-60) 0 = Recent Future Seconds $ negate $ seconds d
        | between (minutes d) 0 60 = Recent Past Minutes $ minutes d
        | between (minutes d) (-60) 0 = Recent Future Minutes $ negate $ minutes d
        | between (hours d) 0 24 = Recent Past Hours $ hours d
        | between (hours d) (-24) 0 = Recent Future Hours $ negate $ hours d
        | between (days d) 0 10 = Recent Past Days $ days d
        | between (days d) (-10) 0 = Recent Future Days $ negate $ days d
        | between (weeks d) 0 5 = Recent Past Weeks $ weeks d
        | between (weeks d) (-5) 0 = Recent Future Weeks $ negate $ weeks d
        | between (years d) 0 1 = ThisYear t
        | between (years d) (-1) 0 = ThisYear t
        | otherwise = Absolute t

    seconds :: NominalDiffTime -> Int
    seconds = truncate

    minutes :: NominalDiffTime -> Int
    minutes s = truncate $ s / 60

    hours :: NominalDiffTime -> Int
    hours s = truncate $ s / (60*60)

    days :: NominalDiffTime -> Int
    days s = truncate $ s / (60*60*24)

    weeks :: NominalDiffTime -> Int
    weeks s = truncate $ s / (60*60*24*7)

    years :: NominalDiffTime -> Int
    years s = truncate $ s / (60*60*24*365) -- eh

    between :: Ord a => a -> a -> a -> Bool
    between d m n = d > m && d < n
