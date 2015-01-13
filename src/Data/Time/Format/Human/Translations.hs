module Data.Time.Format.Human.Translations
    ( Translations(..)
    , def
    ) where

import Data.Default (Default(..))
import Data.Time (TimeZone, utc)
import System.Locale (TimeLocale, defaultTimeLocale)

-- | How to translate phrases used in relative times
--
-- @Bool@ arguments are used to indicate when the date is in the future.
--
-- The @Default@ instance is English.
--
data Translations = Translations
    {
    -- | Used when times are within a second of each other
      justNow :: String

    -- | Used when times are within a minute
    --
    -- TODO: exactly one second apart
    --
    , secondsAgo :: Bool -> String -> String

    -- | Used when times are exactly one minute apart
    , oneMinuteAgo :: Bool -> String

    -- | Used when time are within an hour of each other
    , minutesAgo :: Bool -> String -> String

    -- | Used when times are exactly one hour apart
    , oneHourAgo :: Bool -> String

    -- | Used when times are less than a day apart
    , aboutHoursAgo :: Bool -> String -> String

    -- | Used when times are more than 5 but less than 10 days apart
    --
    -- See also: @'at'@ and @'dayOfWeekFmt'@ records
    --
    , daysAgo :: Bool -> String -> String

    -- | Used when times are exactly one week apart
    , weekAgo :: Bool -> String -> String

    -- | Used when times are less than 5 weeks apart
    , weeksAgo :: Bool -> String -> String

    -- | TODO
    , at :: Int -> String -> String

    -- | TODO
    , onYear :: String -> String

    -- | TODO
    , locale :: TimeLocale

    -- | TODO
    , timeZone :: TimeZone

    -- | TODO
    , dayOfWeekFmt :: String

    -- | TODO
    , thisYearFmt :: String

    -- | TODO
    , prevYearFmt :: String
    }

instance Default Translations where
    def = Translations
        { justNow = "just now"
        , secondsAgo = \f -> (++ " seconds" ++ suffix f)
        , oneMinuteAgo = \f -> "one minute" ++ suffix f
        , minutesAgo = \f -> (++ " minutes" ++ suffix f)
        , oneHourAgo = \f -> "one hour" ++ suffix f
        , aboutHoursAgo = \f x -> "about " ++ x ++ " hours" ++ suffix f
        , at = \_ -> ("at " ++)
        , daysAgo = \f -> (++ " days" ++ suffix f)
        , weekAgo = \f -> (++ " week" ++ suffix f)
        , weeksAgo = \f -> (++ " weeks" ++ suffix f)
        , onYear = ("on " ++)
        , locale = defaultTimeLocale
        , timeZone = utc
        , dayOfWeekFmt  = "%l:%M %p on %A"
        , thisYearFmt = "%b %e"
        , prevYearFmt = "%b %e, %Y"
        }

      where
        suffix True  = " from now"
        suffix False = " ago"
