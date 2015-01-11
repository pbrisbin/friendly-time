module Data.Time.Format.Human.Duration
    ( Duration(..)
    , toDuration
    ) where

import Data.Time (UTCTime)

data Duration = Duration
    { durationValue :: Int }

toDuration :: UTCTime -> UTCTime -> Duration
toDuration _ _ = Duration 0
