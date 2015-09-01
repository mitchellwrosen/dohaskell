module Model.Feed.Internal where

import Prelude

import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP    ((+++), string)
import Text.Read                       (readPrec)

import Database.Persist.TH             (derivePersistField)

data FeedType
    = Atom
    | RSS2
    deriving Eq

instance Show FeedType where
    show Atom = "atom"
    show RSS2 = "rss"

instance Read FeedType where
    readPrec = lift $ (Atom <$ string "atom") +++ (RSS2 <$ string "rss")

derivePersistField "FeedType"
