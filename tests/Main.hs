module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit

import Data.Maybe (fromJust)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.Human
import System.Locale

p :: String -> UTCTime
p = fromJust . parseTime defaultTimeLocale "%F %T"

helper :: String -> String -> String
helper future past = humanReadableTime' (p future) (p past)

helperI18N :: HumanTimeLocale -> String -> String -> String
helperI18N ht future past = humanReadableTimeI18N' ht (p future) (p past)

testJustNow = "just now" @=? helper "2013-08-04 22:30:00"
                                    "2013-08-04 22:30:00"

testSecondsAgo = "13 seconds ago" @=? helper "2013-08-04 22:30:13"
                                             "2013-08-04 22:30:00"

testMinuteAgo = "one minute ago" @=? helper "2013-08-04 22:31:20"
                                            "2013-08-04 22:30:00"

testMinutesAgo = "10 minutes ago" @=? helper "2013-08-04 22:40:00"
                                             "2013-08-04 22:30:00"

testHourAgo = "one hour ago" @=? helper "2013-08-04 23:40:00"
                                        "2013-08-04 22:30:00"

testHoursAgo = "about 4 hours ago" @=? helper "2013-08-05 02:40:00"
                                              "2013-08-04 22:30:00"

testDow = "at 10:30 PM on Sunday" @=? helper "2013-08-08 10:10:10"
                                             "2013-08-04 22:30:00"

testDowEDT = "at 6:30 PM on Sunday" @=?
             helperI18N defaultHumanTimeLocale{timeZone=read "EDT"}
             "2013-08-08 10:10:10"
             "2013-08-04 22:30:00"

testDowIndia = "at 4:00 AM on Monday" @=?
             helperI18N defaultHumanTimeLocale{timeZone=read "+0530"}
             "2013-08-08 10:10:10"
             "2013-08-04 22:30:00"

testDaysAgo = "6 days ago" @=? helper "2013-08-10 22:40:00"
                                      "2013-08-04 22:30:00"

testWeekAgo = "1 week ago" @=? helper "2013-08-16 22:40:00"
                                      "2013-08-04 22:30:00"

testWeeksAgo = "4 weeks ago" @=? helper "2013-09-04 22:40:00"
                                        "2013-08-04 22:30:00"

testThisYear = "on Aug  4" @=? helper "2013-11-04 22:40:00"
                                       "2013-08-04 22:30:00"

testPrevYear = "on Aug  4, 2013" @=? helper "2014-11-04 22:40:00"
                                            "2013-08-04 22:30:00"

tests = [
    testGroup "English friendly time"
        [ testCase "render now" testJustNow
        , testCase "seconds ago" testSecondsAgo
        , testCase "minute ago" testMinuteAgo
        , testCase "minutes ago" testMinutesAgo
        , testCase "hour ago" testHourAgo
        , testCase "hours ago" testHoursAgo
        , testCase "day of week" testDow
        , testCase "day of week in EDT" testDowEDT
        , testCase "day of week in India" testDowIndia
        , testCase "days ago" testDaysAgo
        , testCase "week ago" testWeekAgo
        , testCase "weeks ago" testWeeksAgo
        , testCase "this year" testThisYear
        , testCase "previous year" testPrevYear
        ]
    ]

main :: IO ()
main = defaultMain tests
