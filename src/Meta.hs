module Meta where

import Data.Maybe
import qualified Data.Map as M
import System.IO
import Text.ParserCombinators.Parsec

newtype Meta = Meta (M.Map String [String])
    deriving (Eq, Show)

createMeta = Meta M.empty

getValue :: String -> String -> Meta -> String
getValue key defaultValue meta =
    case getValues key meta of
        []  -> defaultValue
        x:_ -> x

getValues :: String -> Meta -> [String]
getValues key (Meta m) = fromMaybe [] (key `M.lookup` m)

setValue :: String -> String -> Meta -> Meta
setValue key value (Meta m) = setValues key [value] (Meta m)

setValues :: String -> [String] -> Meta -> Meta
setValues key values (Meta m) = Meta $ M.insert key values m

writeMeta :: Meta -> FilePath -> IO ()
writeMeta meta destination = writeFile destination (metaToStr meta)

metaToStr :: Meta -> String
metaToStr (Meta m) = concat $ concatMap entryToLines (M.toList m)
    where
        entryToLines (key, values) = map (entryToLine key) values
        entryToLine key value = key ++ "::" ++ value ++ "\n"

metaFromStr :: String -> Meta
metaFromStr str = Meta $ M.fromListWith reverseAppend (map (\(a, b) -> (a, [b])) (parseMeta str))
    where
        reverseAppend a b = b ++ a

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
