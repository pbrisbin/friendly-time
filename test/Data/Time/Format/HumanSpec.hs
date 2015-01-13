module Data.Time.Format.HumanSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Data.Time.Format.Human

import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "formatHumanReadable'" $ do
    it "returns just now for near times" $ do
        let n = parseTime' "2015-01-01 01:00:00.1"
            t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "just now"
        formatHumanReadable' t n `shouldBe` "just now"

    it "returns seconds for times less than a minute" $ do
        let n = parseTime' "2015-01-01 01:00:59"
        let t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "59 seconds ago"
        formatHumanReadable' t n `shouldBe` "59 seconds from now"

    it "returns minutes for times less than an hour" $ do
        let n = parseTime' "2015-01-01 01:59:00"
        let t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "59 minutes ago"
        formatHumanReadable' t n `shouldBe` "59 minutes from now"

    it "returns hours for times less than a day" $ do
        let n = parseTime' "2015-01-01 23:59:00"
        let t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "about 22 hours ago"
        formatHumanReadable' t n `shouldBe` "about 22 hours from now"

    context "when less than 5 days " $ do
        let n = parseTime' "2015-01-04 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        it "returns day of week in EDT" $ do
            let l = def { timeZone = read "EDT" }

            formatHumanReadableWith' l n t `shouldBe` "at 9:00 PM on Wednesday"
            formatHumanReadableWith' l t n `shouldBe` "at 9:00 PM on Saturday"

        it "returns day of week in India" $ do
            let l = def { timeZone = read "+0530" }

            formatHumanReadableWith' l n t `shouldBe` "at 6:30 AM on Thursday"
            formatHumanReadableWith' l t n `shouldBe` "at 6:30 AM on Sunday"

    it "returns days for times less than 10 days" $ do
        let n = parseTime' "2015-01-10 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "9 days ago"
        formatHumanReadable' t n `shouldBe` "9 days from now"

    it "returns weeks for times less than 5 weeks" $ do
        let n = parseTime' "2015-01-29 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "4 weeks ago"
        formatHumanReadable' t n `shouldBe` "4 weeks from now"

    it "returns a date string without year for this year" $ do
        let n = parseTime' "2015-12-30 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "on Jan  1" -- TODO: spacing
        formatHumanReadable' t n `shouldBe` "on Dec 30"

    it "returns a date string including the year for previous years" $ do
        let n = parseTime' "2025-12-30 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        formatHumanReadable' n t `shouldBe` "on Jan  1, 2015" -- TODO: spacing
        formatHumanReadable' t n `shouldBe` "on Dec 30, 2025"

parseTime' :: String -> UTCTime
parseTime' = fromJust . parseTime defaultTimeLocale "%F %T%Q"
