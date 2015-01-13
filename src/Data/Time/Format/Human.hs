{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- |
--
-- Print a @'UTCTime'@ as "a few seconds ago" or "3 days ago" and similar.
--
-------------------------------------------------------------------------------
module Data.Time.Format.Human
    (
    -- * IO functions for formatting times relative to "now"
      formatHumanReadable
    , formatHumanReadableWith

    -- * Pure versions for formatting times relative to another time
    , formatHumanReadable'
    , formatHumanReadableWith'

    -- * For customizing via the @With@ functions
    , module Data.Time.Format.Human.Translations
    ) where

import Data.Time.Format.Human.Translations

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import Data.Time
    ( NominalDiffTime
    , UTCTime
    , diffUTCTime
    , formatTime
    , getCurrentTime
    , utcToLocalTime
    )
import System.Locale (defaultTimeLocale)

-- | Format time relative to now with default (english) translations
formatHumanReadable :: MonadIO m => UTCTime -> m String
formatHumanReadable = formatHumanReadableWith def

-- | Format time relative to now with the given translations
formatHumanReadableWith :: MonadIO m => Translations -> UTCTime -> m String
formatHumanReadableWith tl t = do
    now <- liftIO $ getCurrentTime
    return $ formatHumanReadableWith' tl now t

-- | Format times relative to each other
formatHumanReadable' :: UTCTime -- ^ Taken as "now" w.r.t. "ago" vs "from now"
                     -> UTCTime -> String
formatHumanReadable' = formatHumanReadableWith' def

-- | Format times relative to each other with the given translations
formatHumanReadableWith' :: Translations -> UTCTime -> UTCTime -> String
formatHumanReadableWith' (Translations{..}) cur t = helper $ diffUTCTime cur t
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
