{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- |
--
-- Print a @'UTCTime'@ as "a few seconds ago" or "3 days ago" and similar.
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

#if !MIN_VERSION_time(1,5,0)
import System.Locale (TimeLocale, defaultTimeLocale)
#endif


data HumanTimeLocale = HumanTimeLocale
    { justNow       :: String
    , secondsAgo    :: Bool -> String -> String
    , oneMinuteAgo  :: Bool -> String
    , minutesAgo    :: Bool -> String -> String
    , oneHourAgo    :: Bool -> String
    , aboutHoursAgo :: Bool -> String -> String
    -- | Used when time difference is more than 24 hours but less than 5 days.
    -- First argument is the day of week of the older time, second is string
    -- formatted with `dayOfWeekFmt`.
    , at            :: Int -> String -> String
    , daysAgo       :: Bool -> String -> String
    , weekAgo       :: Bool -> String -> String
    , weeksAgo      :: Bool -> String -> String
    , onYear        :: String -> String
    , locale        :: TimeLocale
    , timeZone      :: TimeZone
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
    , secondsAgo    = \f -> (++ " seconds" ++ dir f)
    , oneMinuteAgo  = \f -> "one minute" ++ dir f
    , minutesAgo    = \f -> (++ " minutes" ++ dir f)
    , oneHourAgo    = \f -> "one hour" ++ dir f
    , aboutHoursAgo = \f x -> "about " ++ x ++ " hours" ++ dir f
    , at            = \_ -> ("at " ++)
    , daysAgo       = \f -> (++ " days" ++ dir f)
    , weekAgo       = \f -> (++ " week" ++ dir f)
    , weeksAgo      = \f -> (++ " weeks" ++ dir f)
    , onYear        = ("on " ++)
    , locale        = defaultTimeLocale
    , timeZone      = utc
    , dayOfWeekFmt  = "%l:%M %p on %A"
    , thisYearFmt   = "%b %e"
    , prevYearFmt   = "%b %e, %Y"
    }
    where dir True  = " from now"
          dir False = " ago"


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

        oldDayOfWeek :: Int
        oldDayOfWeek = read $ formatTime defaultTimeLocale "%u" t

        old           = utcToLocalTime timeZone t
        format        = formatTime locale
        dow           = trim $! format dayOfWeekFmt old
        thisYear      = trim $! format thisYearFmt old
        previousYears = trim $! format prevYearFmt old

        helper d = helper' (d < 0) (abs d)

        helper' future d
            | d         < 1  = justNow
            | d         < 60 = secondsAgo future $ i2s d
            | minutes d < 2  = oneMinuteAgo future
            | minutes d < 60 = minutesAgo future $ i2s (minutes d)
            | hours d   < 2  = oneHourAgo future
            | hours d   < 24 = aboutHoursAgo future $ i2s (hours d)
            | days d    < 5  = at oldDayOfWeek dow
            | days d    < 10 = daysAgo future $ i2s (days d)
            | weeks d   < 2  = weekAgo future $ i2s (weeks d)
            | weeks d   < 5  = weeksAgo future $ i2s (weeks d)
            | years d   < 1  = onYear thisYear
            | otherwise      = onYear previousYears
