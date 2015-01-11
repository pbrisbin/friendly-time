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
        let (from, to) = (makeTime "1:00.000", makeTime "1:00.005")

        durationValue (toDuration from to) `shouldBe` 0

makeTime :: String -> UTCTime
makeTime = undefined
