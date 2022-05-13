{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( scrapeRaceResults
    , resultsToCsv
    ) where
import Text.HTML.Scalpel
import Data.Functor.Identity (Identity)
import Debug.Trace
import Control.Monad
import GHC.Generics (Generic)
import Data.Csv (ToRecord, FromRecord, encode)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 (unpack)

url = "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=40473"

removeNothing :: Maybe a -> [a]
removeNothing (Just a) = [a]
removeNothing Nothing = []

scrapeRaceResults :: URL -> IO (Maybe [RaceResult])
scrapeRaceResults url = fmap (concatMap removeNothing) <$> scrapeURL url scoreRows where
    scoreRows = chroots (
        "div" @: [hasClass "tbody"] //
        "a" @: [hasClass "table-row"] //
        "div" @: [hasClass "g-row", hasClass "container"])
        result

    result :: ScraperT String Identity (Maybe RaceResult) = do
        rawVals <- chroots ("div" // "div" // "div") $ do
            index <- position
            case index of
              n | n == 5 -> text $ "span" @: [hasClass "country__name-short"]
              _ -> text anySelector
        if length rawVals > 10 then
            traceM $ "warning! more than 10 columns found! did the website change? at: " <> show rawVals
        else pure ()
        let vals = take 9 $ fmap (unwords . words) rawVals
        traceShowM vals
        pure $ resultFromList vals

data RaceResult = RaceResult String String String String String String String String String
    deriving (Show, Eq, Generic)
instance ToRecord RaceResult
instance FromRecord RaceResult

resultFromList [rank, bib, fiscode, name, year, country, time, diffTime, fisPoints] =
    Just $ RaceResult rank bib fiscode name year country time diffTime fisPoints
resultFromList _ = Nothing

resultsToCsv :: [RaceResult] -> String
resultsToCsv results =
    unpack (encode results)
