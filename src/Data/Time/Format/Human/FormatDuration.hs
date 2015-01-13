module Data.Time.Format.Human.FormatDuration
    ( formatDurationEn
    ) where

import Data.Time.Format.Human.Duration
import Data.Time.Format.Human.Locale

formatDurationEn :: Duration -> String
formatDurationEn (Duration _ _ 0) = "just now"
formatDurationEn (Duration Past Seconds 1) = "one second ago"
formatDurationEn (Duration Past Seconds s) = show s ++ " seconds ago"
formatDurationEn (Duration Future Seconds 1) = "one second from now"
formatDurationEn (Duration Future Seconds s) = show s ++ " seconds from now"
formatDurationEn (Duration Past Minutes 1) = "one minute ago"
formatDurationEn (Duration Past Minutes m) = show m ++ " minutes ago"
formatDurationEn (Duration Future Minutes 1) = "one minute from now"
formatDurationEn (Duration Future Minutes m) = show m ++ " minutes from now"
formatDurationEn (Duration Past Hours 1) = "one hour ago"
formatDurationEn (Duration Past Hours h) = "about " ++ show h ++ " hours ago"
formatDurationEn (Duration Future Hours 1) = "one hour from now"
formatDurationEn (Duration Future Hours h) = "about " ++ show h ++ " hours from now"
formatDurationEn (Duration Past Days 1) = "one day ago"
formatDurationEn (Duration Past Days d) = show d ++ " days ago"
formatDurationEn (Duration Future Days 1) = "one day from now"
formatDurationEn (Duration Future Days d) = show d ++ " days from now"
formatDurationEn (Duration Past Weeks 1) = "one week ago"
formatDurationEn (Duration Past Weeks w) = show w ++ " weeks ago"
formatDurationEn (Duration Future Weeks 1) = "one week from now"
formatDurationEn (Duration Future Weeks w) = show w ++ " weeks from now"
formatDurationEn (Absolute t) = formatTime defaultTimeLocale "%b %d, %Y" t

{-
    at            = \_ -> ("at " ++)
    onYear        = ("on " ++)
    dayOfWeekFmt  = "%l:%M %p on %A"
    thisYearFmt   = "%b %e"
    prevYearFmt   = "%b %e, %Y"
-}
