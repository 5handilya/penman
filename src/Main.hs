module Main where

import Types (SourceFile(..))
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List.Split(splitOn)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension)
import Control.Monad (forM_)

parsePath :: FilePath -> Maybe SourceFile
parsePath path =
    if takeExtension path /= ".md"
       then Nothing
       else
        let baseName = takeBaseName path
            parts = splitOn "-" baseName
            input = head parts  -- input = 'PxCy' i.e. the encoded part of filename baseName to be parsed
        in
            if 'P' `elem` input && 'C' `elem` input
               then
                let partStr = tail $ takeWhile (/= 'C') input
                    chapterStr = tail $ dropWhile (/= 'C') input
                    maybePart = readMaybe partStr
                    maybeChapter = readMaybe chapterStr
                in
                    --combining both maybes into one
                    SourceFile <$> maybePart <*> maybeChapter <*> Just path
               else Nothing

main :: IO ()
main = do
    let contentDir = "content"
    putStrlLn $ "Searching for content in: ./" ++ contentDir
    allItems <- listDirectory contentDir
    putStrlLn $ "\nFound these items: " ++ show allItems

    let parsedItems = map parsePath allItems
    putstrlLn $ "\nParsed items (nothing means invalid format): " ++ show parsedItems

    let validFiles = catMaybes parsedItems
    putStrLn "\nSuccessfully parsed these valid files:"

    forM_ validFiles $ \file ->
    putStrLn $ "  - Part: " ++ show (sfPart file) ++
    ", Chapter: " ++ show (sfChapter file) ++
    ", Path: " ++ sfFilePath file
