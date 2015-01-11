module Data.Time.Format.Human.DurationSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Data.Time.Format.Human.Duration

import Data.Time (UTCTime)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Duration" $ do
    it "produces zero duration for near values" $ do
        let (from, to) = makeTimes "1:00:00.000" "1:00:00.100"

        durationValue (toDuration from to) `shouldBe` 0

    it "produces seconds in past for duration of less than a minute" $ do
        let (from, to) = makeTimes "1:00:59.000" "1:00:00.000"

        toDuration from to `shouldBe` Duration Past Seconds 59

makeTimes :: String -> String -> (UTCTime, UTCTime)
makeTimes = undefined
