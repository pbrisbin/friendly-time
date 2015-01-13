module Data.Format.Time.Human.Locale
    ( HumanTimeLocale(..)
    , defaultHumanTimeLocale
    ) where

data HumanTimeLocale = HumanTimeLocale
    { justNow       :: String
    , secondsAgo    :: String -> String
    , oneMinuteAgo  :: String
    , minutesAgo    :: String -> String
    , oneHourAgo    :: String
    , aboutHoursAgo :: String -> String
    -- | Used when time difference is more than 24 hours but less than 5 days.
    -- First argument is the day of week of the older time, second is string
    -- formatted with `dayOfWeekFmt`.
    , at            :: Int -> String -> String
    , daysAgo       :: String -> String
    , weekAgo       :: String -> String
    , weeksAgo      :: String -> String
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
    , secondsAgo    = (++ " seconds ago")
    , oneMinuteAgo  = "one minute ago"
    , minutesAgo    = (++ " minutes ago")
    , oneHourAgo    = "one hour ago"
    , aboutHoursAgo = \x -> "about " ++ x ++ " hours ago"
    , at            = \_ -> ("at " ++)
    , daysAgo       = (++ " days ago")
    , weekAgo       = (++ " week ago")
    , weeksAgo      = (++ " weeks ago")
    , onYear        = ("on " ++)
    , locale        = defaultTimeLocale
    , timeZone      = utc
    , dayOfWeekFmt  = "%l:%M %p on %A"
    , thisYearFmt   = "%b %e"
    , prevYearFmt   = "%b %e, %Y"
    }
