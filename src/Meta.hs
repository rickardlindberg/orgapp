module Meta
    ( Meta(..)
    , createMeta
    ) where

data Meta = Meta {
    foo :: [String]
}

createMeta = Meta []
