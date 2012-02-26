module Meta where

import qualified Data.Map as M
import System.IO
import Text.ParserCombinators.Parsec

newtype Meta = Meta (M.Map String String)
    deriving (Eq, Show)

createMeta = Meta M.empty
metaFilename = getValue "name" ""
setFilename = setValue "name"

writeMeta :: Meta -> FilePath -> IO ()
writeMeta meta destination = writeFile destination (metaToStr meta)

metaToStr :: Meta -> String
metaToStr (Meta m) = concat $ map (\(a, b) -> a ++ "::" ++ b ++ "\n") (M.toList m)

metaFromStr :: String -> Meta
metaFromStr str = Meta $ M.fromList (parseMeta str)

getValue :: String -> String -> Meta -> String
getValue key defaultValue (Meta m) =
    case key `M.lookup` m of
        Nothing -> defaultValue
        Just x  -> x

setValue :: String -> String -> Meta -> Meta
setValue key value (Meta m) = Meta $ M.insert key value m

parseMeta :: String -> [(String, String)]
parseMeta input =
    case parse file "" input of
        Left _  -> []
        Right x -> x
    where
        file :: Parser [(String, String)]
        file  = do
            lines <- many line
            eof
            return lines
        line  = do
            key <- key
            sep
            value <- value
            eol
            return (key, value)
        key   = many (noneOf ":")
        sep   = string "::"
        value = many (noneOf "\n")
        eol   = char '\n'
