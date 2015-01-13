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
spec = describe "humanReadableTIme'" $ do
    it "returns just now for near times" pending
    it "returns seconds for times less than a minute" pending
    it "returns minutes for times less than an hour" pending
    it "returns hours for times less than a day" pending

    context "when less than 5 days " $ do
        it "returns day of week" pending
        it "returns day of week in EDT" pending
        it "returns day of week in India" pending

    it "returns days for times less than 10 days" pending
    it "returns weeks for times less than 5 weeks" pending

    context "this year" $ do
        it "returns a date string without year" pending

    context "previous years" $ do
        it "returns a date string including the year" pending

parseTime' :: String -> UTCTime
parseTime' = fromJust . parseTime defaultTimeLocale "%F %T"
