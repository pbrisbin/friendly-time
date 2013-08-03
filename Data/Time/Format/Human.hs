{-# LANGUAGE RecordWildCards #-}
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
-- Prints a @'UTCTime'@ as "a few seconds ago" or "3 days ago" and similar.
--
-------------------------------------------------------------------------------
module Data.Time.Format.Human
    ( humanReadableTime
    , humanReadableTime'
    , humanReadableTimeI18N
    , humanReadableTimeI18N'
    , HumanTimeLocale(..)
    , defaultHumanTimeLocale
    ) where

import Data.Time

import Data.Char (isSpace)
import System.Locale

data HumanTimeLocale = HumanTimeLocale
    { justNow       :: String
    , secondsAgo    :: String -> String
    , oneMinuteAgo  :: String
    , minutesAgo    :: String -> String
    , oneHourAgo    :: String
    , aboutHoursAgo :: String -> String
    -- | Used when time difference is more than 24 hours but less than 5 days.
    , at            :: String -> String
    , daysAgo       :: String -> String
    , weekAgo       :: String -> String
    , weeksAgo      :: String -> String
    , onYear        :: String -> String
    , locale        :: TimeLocale
    -- | Time format used with `at` member. See @Data.Time.Format@ for
    --   details on formatting  sequences.
    , dayOfWeekFmt  :: String
    -- | Time format used when time difference is less than a year but more
    --   than a month. Time formatted using this string will be passed
    --   to `onYear`.
    , thisYearFmt   :: String
    -- | Time format used when time difference is at least one year. Time
    --   formatted using this string will be passed to `onYear`.
    , prevYearFmt   :: String
    }

-- | Default human time locale uses English.
defaultHumanTimeLocale :: HumanTimeLocale
defaultHumanTimeLocale = HumanTimeLocale
    { justNow       = "just now"
    , secondsAgo    = (++ " seconds ago")
    , oneMinuteAgo  = "one minute ago"
    , minutesAgo    = (++ " minutes ago")
    , oneHourAgo    = "one hour ago"
    , aboutHoursAgo = \x -> "about " ++ x ++ " hours ago"
    , at            = ("at " ++)
    , daysAgo       = (++ " days ago")
    , weekAgo       = (++ " week ago")
    , weeksAgo      = (++ " weeks ago")
    , onYear        = ("on " ++)
    , locale        = defaultTimeLocale
    , dayOfWeekFmt  = "%l:%M %p on %A"
    , thisYearFmt   = "%b %e"
    , prevYearFmt   = "%b %e, %Y"
    }

-- | Based on @humanReadableTimeDiff@ found in
--   <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs>,
--   <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTime :: UTCTime -> IO String
humanReadableTime = humanReadableTimeI18N defaultHumanTimeLocale

-- | A pure form, takes current time as an argument
humanReadableTime' :: UTCTime -- ^ current time
                   -> UTCTime -> String
humanReadableTime' = humanReadableTimeI18N' defaultHumanTimeLocale

-- | I18N version of `humanReadableTime`
humanReadableTimeI18N :: HumanTimeLocale -> UTCTime -> IO String
humanReadableTimeI18N tl t = do
    now <- getCurrentTime
    return $ humanReadableTimeI18N' tl now t

-- | I18N version of `humanReadableTime'`
humanReadableTimeI18N' :: HumanTimeLocale
                       -> UTCTime -- ^ current time
                       -> UTCTime -> String
humanReadableTimeI18N' (HumanTimeLocale {..}) cur t = helper $ diffUTCTime cur t
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
        format        = formatTime locale
        dow           = trim $! format dayOfWeekFmt old
        thisYear      = trim $! format thisYearFmt old
        previousYears = trim $! format prevYearFmt old

        helper d
            | d         < 1  = justNow
            | d         < 60 = secondsAgo $ i2s d
            | minutes d < 2  = oneMinuteAgo
            | minutes d < 60 = minutesAgo $ i2s (minutes d)
            | hours d   < 2  = oneHourAgo
            | hours d   < 24 = aboutHoursAgo $ i2s (hours d)
            | days d    < 5  = at dow
            | days d    < 10 = daysAgo $ i2s (days d)
            | weeks d   < 2  = weekAgo $ i2s (weeks d)
            | weeks d   < 5  = weeksAgo $ i2s (weeks d)
            | years d   < 1  = onYear thisYear
            | otherwise      = onYear previousYears
