-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Format.Human
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Prints a @'UTCTime'@ as "a few seconds ago" or "3 days ago" and
-- similar.
--
-------------------------------------------------------------------------------
module Data.Time.Format.Human
    ( humanReadableTime
    , humanReadableTime'
    ) where

import Data.Time

import Data.Char (isSpace)
import System.Locale (defaultTimeLocale)

-- | Based on @humanReadableTimeDiff@ found in
--   <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs>,
--   <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTime :: UTCTime -> IO String
humanReadableTime t = do
    now <- getCurrentTime
    return $ humanReadableTime' now t

-- | A pure form, takes the current time as an argument
humanReadableTime' :: UTCTime -- ^ current time
                   -> UTCTime -> String
humanReadableTime' cur t = helper $ diffUTCTime cur t

    where
        minutes :: NominalDiffTime -> Double
        minutes n = realToFrac $ n / 60

        hours :: NominalDiffTime -> Double
        hours n = minutes n / 60

        days :: NominalDiffTime -> Double
        days n = hours n / 24

        weeks :: NominalDiffTime -> Double
        weeks n = days n / 7

        years :: NominalDiffTime -> Double
        years n = days n / 365

        i2s :: RealFrac a => a -> String
        i2s n = show m where m = truncate n :: Int

        trim = f . f where f = reverse . dropWhile isSpace

        old           = utcToLocalTime utc t
        dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
        thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
        previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

        helper d 
            | d         < 1  = "just now"
            | d         < 60 = i2s d ++ " seconds ago"
            | minutes d < 2  = "one minute ago"
            | minutes d < 60 =  i2s (minutes d) ++ " minutes ago"
            | hours d   < 2  = "one hour ago"
            | hours d   < 24 = "about " ++ i2s (hours d) ++ " hours ago"
            | days d    < 5  = "at " ++ dow
            | days d    < 10 = i2s (days d)  ++ " days ago"
            | weeks d   < 2  = i2s (weeks d) ++ " week ago"
            | weeks d   < 5  = i2s (weeks d) ++ " weeks ago"
            | years d   < 1  = "on " ++ thisYear
            | otherwise      = "on " ++ previousYears
