module Data.Time.Format.Human.DurationSpec
    ( main
    , spec
    ) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Duration" $ do
    it "works" $ do
        1 + 1 `shouldBe` 2
