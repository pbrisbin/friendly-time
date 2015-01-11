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

        durationValue (toDuration n t) `shouldBe` 0

    it "produces seconds in past for duration of less than a minute" $ do
        let n = parseTime' "2015-01-01 01:00:59"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Duration Past Seconds 59

    it "produces minutes in past for duration of less than an hour" $ do
        let n = parseTime' "2015-01-01 01:59:00"
        let t = parseTime' "2015-01-01 01:00:00"

        toDuration n t `shouldBe` Duration Past Minutes 59

parseTime' :: String -> UTCTime
parseTime' = fromJust . parseTime defaultTimeLocale "%F %T%Q"
