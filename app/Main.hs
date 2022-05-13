module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    [url, outFile] <- getArgs
    maybeResults <- scrapeRaceResults url
    case maybeResults of
        Nothing ->
            error "no results...?"
        Just results ->
            writeFile outFile (resultsToCsv results)
