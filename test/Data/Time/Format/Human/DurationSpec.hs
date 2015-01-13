module Data.Time.Format.Human.DurationSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Data.Time.Format.Human.Duration

import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Duration" $ do
    it "produces zero duration for near values" $ do
        let n = parseTime' "2015-01-01 01:00:00"
            t = parseTime' "2015-01-01 01:00:00.1"

        durationValue (toDuration n t) `shouldBe` Just 0

    it "produces seconds for duration of less than a minute" $ do
        let n = parseTime' "2015-01-01 01:00:59"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Recent Past Seconds 59
        toDuration t n `shouldBe` Recent Future Seconds 59

    it "produces minutes for duration of less than an hour" $ do
        let n = parseTime' "2015-01-01 01:59:00"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Recent Past Minutes 59
        toDuration t n `shouldBe` Recent Future Minutes 59

    it "produces hours for duration of less than one day" $ do
        let n = parseTime' "2015-01-01 23:59:00"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Recent Past Hours 22
        toDuration t n `shouldBe` Recent Future Hours 22

    it "produces day of week if less than 5 days" $ do
        pendingWith "At X on Y"

    it "produces days for duration of less than 10 days" $ do
        let n = parseTime' "2015-01-10 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Recent Past Days 9
        toDuration t n `shouldBe` Recent Future Days 9

    it "produces weeks for duration of less than 5 weeks" $ do
        let n = parseTime' "2015-01-30 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Recent Past Weeks 4
        toDuration t n `shouldBe` Recent Future Weeks 4

    it "produces this year for duration of less than 1 year" $ do
        let n = parseTime' "2015-12-30 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` ThisYear t
        toDuration t n `shouldBe` ThisYear n

    it "produces absolute values for older dates" $ do
        let n = parseTime' "2025-01-01 01:00:00"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Absolute t
        toDuration t n `shouldBe` Absolute n

parseTime' :: String -> UTCTime
parseTime' = fromJust . parseTime defaultTimeLocale "%F %T%Q"
