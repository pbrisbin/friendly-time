module Data.Time.Format.Human.Locales
    ( spanishHumanTimeLocale
    ) where

import Data.Time
import Data.Time.Format.Human

-- | Spanish human time locale.
spanishHumanTimeLocale :: HumanTimeLocale
spanishHumanTimeLocale = HumanTimeLocale
    { justNow = "Justo ahora"
    , secondsAgo = \f x -> dir f ++ " " ++ x ++ " segundos"
    , oneMinuteAgo = \f -> dir f ++ " un minuto"
    , minutesAgo = \f x -> dir f ++ " " ++ x ++ " minutos"
    , oneHourAgo = \f -> dir f ++ " una hora"
    , aboutHoursAgo = \f x -> dir f ++ " aproximadamente " ++ x ++ " horas"
    , at = const ("El " ++)
    , daysAgo = \f x -> dir f ++ " " ++ x ++ " dias"
    , weekAgo = \f x -> dir f ++ " " ++ x ++ " semana"
    , weeksAgo = \f x -> dir f ++ " " ++ x ++ " semanas"
    , onYear = ("En " ++)
    , locale = spanishTimeLocale
    , timeZone = utc
    , dayOfWeekFmt = "%A a las %l:%M %p"
    , thisYearFmt = "%b/%e"
    , prevYearFmt = "%Y/%b/%e"
    }
  where
    dir True = "Dentro de"
    dir False = "Hace"
    spanishTimeLocale = TimeLocale
        { wDays = [ ("Domingo", "Dom")
                  , ("Lunes", "Lun")
                  , ("Martes", "Mar")
                  , ("Miercoles", "Mie")
                  , ("Jueves", "Jue")
                  , ("Viernes", "Vie")
                  , ("Sabado", "Sab")
                  ]
        , months = [ ("Enero", "Ene")
                   , ("Febrero", "Feb")
                   , ("Marzo", "Mar")
                   , ("Abril", "Abr")
                   , ("Mayo", "May")
                   , ("Junio", "Jun")
                   , ("Julio", "Jul")
                   , ("Agosto", "Ago")
                   , ("Septiembre", "Sep")
                   , ("Octubre", "Oct")
                   , ("Noviembre", "Nov")
                   , ("Diciembre", "Dec")
                   ]
        , amPm = ("AM", "PM")
        , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
        , dateFmt = "%y/%m/%d"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones = [ TimeZone 0 False "UT"
                           , TimeZone 0 False "GMT"
                           , TimeZone (-5 * 60) False "EST"
                           , TimeZone (-4 * 60) True "EDT"
                           , TimeZone (-6 * 60) False "CST"
                           , TimeZone (-5 * 60) True "CDT"
                           , TimeZone (-7 * 60) False "MST"
                           , TimeZone (-6 * 60) True "MDT"
                           , TimeZone (-8 * 60) False "PST"
                           , TimeZone (-7 * 60) True "PDT"
                           ]
        }
