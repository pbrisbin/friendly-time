-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Format.Human
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Prints a @'UTCTime'@ as "a few seconds ago" or "3 days ago" and
-- similar.
--
-------------------------------------------------------------------------------
module Data.Time.Format.Human
    ( humanReadableTime
    , humanReadableTime'
    , defaultHumanTimeLocale
    ) where

import Data.Time

import Data.Char (isSpace)
import System.Locale


data HumanTimeLocale = HumanTimeLocale {
		htintervals :: [(String, String)],
		htlocale :: TimeLocale
	} deriving (Eq, Ord, Show)


defaultHumanTimeLocale :: HumanTimeLocale
defaultHumanTimeLocale = HumanTimeLocale {
	htintervals = [ (""," just now")
	              , (""," " ++ (snd $ (intervals defaultTimeLocale)!!5) ++ " ago")
	              , (""," one " ++ (fst $ (intervals defaultTimeLocale)!!4) ++ " ago")
	              , (""," " ++ (snd $ (intervals defaultTimeLocale)!!4) ++ " ago")
	              , (""," one " ++ (fst $ (intervals defaultTimeLocale)!!3) ++ " ago")
	              , ("about "," " ++ (snd $ (intervals defaultTimeLocale)!!3) ++ " ago")
	              , ("at ","")
	              , (""," " ++ (snd $ (intervals defaultTimeLocale)!!2) ++ " ago")
	              , (""," week ago")  -- !! 8
	              , (""," weeks ago")
	              , ("on ","")
	              , ("on ","")
	              ] ,
	htlocale = defaultTimeLocale }
	


-- | Based on @humanReadableTimeDiff@ found in
--   <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs>,
--   <https://github.com/snoyberg/haskellers/blob/master/LICENSE>

humanReadableTime :: HumanTimeLocale -> UTCTime -> IO String
humanReadableTime tl t = do
    now <- getCurrentTime
    return $ humanReadableTime' tl now t

-- | A pure form, takes the current time as an argument
humanReadableTime' :: HumanTimeLocale
                   -> UTCTime -- ^ current time
                   -> UTCTime -> String
humanReadableTime' tl cur t = helper $ diffUTCTime cur t

    where
        minutes :: NominalDiffTime -> Double
        minutes n = realToFrac $ n / 60

        hours :: NominalDiffTime -> Double
        hours n = minutes n / 60

        days :: NominalDiffTime -> Double
        days n = hours n / 24

        weeks :: NominalDiffTime -> Double
        weeks n = days n / 7

        years :: NominalDiffTime -> Double
        years n = days n / 365

        i2s :: RealFrac a => a -> String
        i2s n = show m where m = truncate n :: Int

        trim = f . f where f = reverse . dropWhile isSpace

        old           = utcToLocalTime utc t
        dow           = trim $! formatTime (htlocale tl) "%l:%M %p on %A" old
        thisYear      = trim $! formatTime (htlocale tl) "%b %e" old
        previousYears = trim $! formatTime (htlocale tl) "%b %e, %Y" old

        helper d 
            | d         < 1  = (snd $ htintervals tl !! 0)
            | d         < 60 = i2s d ++ (snd $ htintervals tl !! 1)
            | minutes d < 2  = (snd $ htintervals tl !! 2)
            | minutes d < 60 =  i2s (minutes d) ++ (snd $ htintervals tl !! 3)
            | hours d   < 2  = (snd $ htintervals tl !! 4)
            | hours d   < 24 = (fst $ htintervals tl !! 5) ++ i2s (hours d) ++ (snd $ htintervals tl !! 5)
            | days d    < 5  = (fst $ htintervals tl !! 6) ++ dow ++ (snd $ htintervals tl !! 6)
            | days d    < 10 = i2s (days d) ++ (snd $ htintervals tl !! 7)
            | weeks d   < 2  = (fst $ htintervals tl !! 8) ++ i2s (weeks d) ++ (snd $ htintervals tl !! 8)
            | weeks d   < 5  = i2s (weeks d) ++ (snd $ htintervals tl !! 9)
            | years d   < 1  = (fst $ htintervals tl !! 10) ++ thisYear ++ (snd $ htintervals tl !! 10)
            | otherwise      = (fst $ htintervals tl !! 11) ++ previousYears
