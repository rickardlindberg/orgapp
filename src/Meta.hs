module Meta where

import Data.Maybe
import qualified Data.Map as M
import System.IO
import Text.ParserCombinators.Parsec

newtype Meta = Meta (M.Map String String)
    deriving (Eq, Show)

createMeta = Meta M.empty

getValue :: String -> String -> Meta -> String
getValue key defaultValue (Meta m) = fromMaybe defaultValue (key `M.lookup` m)

setValue :: String -> String -> Meta -> Meta
setValue key value (Meta m) = Meta $ M.insert key value m

writeMeta :: Meta -> FilePath -> IO ()
writeMeta meta destination = writeFile destination (metaToStr meta)

metaToStr :: Meta -> String
metaToStr (Meta m) = concatMap entryToLine (M.toList m)
    where
        entryToLine (key, value) = key ++ "::" ++ value ++ "\n"

metaFromStr :: String -> Meta
metaFromStr str = Meta $ M.fromList (parseMeta str)

parseMeta :: String -> [(String, String)]
parseMeta input =
    case parse file "" input of
        Left  _ -> []
        Right x -> x
    where
        file = do
            lines <- many line
            eof
            return lines
        line = do
            key <- key
            sep
            value <- value
            eol
            return (key, value)
        key   = many (noneOf ":")
        sep   = string "::"
        value = many (noneOf "\n")
        eol   = char '\n'
