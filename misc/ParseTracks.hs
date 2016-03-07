{-# LANGUAGE ImplicitParams #-}

module ParseTracks
    ( Song(..)
    , parseNummersDBs
    ) where

import Prelude hiding (getContents, putStr, readFile, print)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine) 
import Data.Encoding.ISO88591
import System.IO.Encoding
import Time

{-
main = do 
    let ?enc = ISO88591
    content <- readFile "Nummers.dbs"
    print $ content ++ "\r\n"
-}

parseNummersDBs :: IO [Song]
parseNummersDBs = do
    let ?enc = ISO88591
    content <- readFile "Nummers.dbs"
    return . extract__ . parse nummersFile "(stdout)" $ content ++ "\r\n"
        where   extract__ (Right y) = y 
                extract__ (Left x)  = error $ "could not parse: " ++ show x  

{-
parseNummers :: String -> Either ParseError [Song]
parseNummers = parse nummersFile "(unknown)"
-}

data Song = Song    { group :: String
                    , album :: String
                    , year :: Int
                    , track :: Int
                    , title :: String
                    , tags :: [String]
                    , length :: Time
                    , country :: [String]
                    } deriving (Show, Eq, Ord)


nummersFile = do
    -- handle header
    string "8" >> endOfLine >> string "Artist" >> endOfLine
                            >> string "CD" >> endOfLine
                            >> string "Year" >> endOfLine
                            >> string "Track nr" >> endOfLine
                            >> string "Track title" >> endOfLine
                            >> string "Tags" >> endOfLine
                            >> string "Length" >> endOfLine
                            >> string "Country" >> endOfLine
                            >> endOfLine
    -- handle records
    endBy record (endOfLine >> endOfLine)



record = do
    group <- line
    endOfLine
    album <- line
    endOfLine
    year <- int
    endOfLine
    track <- int
    endOfLine
    title <- line
    endOfLine
    tags <- csv_line    
    endOfLine
    min <- int
    char ':'
    sec <- int
    endOfLine
    country <- csv_line
    return (Song group album year track title tags (toTime min sec) country)
        where   int = read <$> many digit
                line = many (noneOf "\r\n")
                csv_line = sepBy (many (noneOf ",\r\n")) (char ',')


