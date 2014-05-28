module Model.ResourceType where

import Prelude

import Database.Persist.TH (derivePersistField)
import Data.Text           (Text)
import Text.Blaze          (ToMarkup, preEscapedToMarkup, toMarkup)

data ResourceType 
    = BlogPost
    | ForumPost
    | LectureNotes
    | QAWebsite
    | ResearchPaper
    | SourceCode
    | Textbook
    | VideoLecture
    deriving (Bounded, Enum, Eq, Read, Show)
derivePersistField "ResourceType"

-- Reuse instance ToMarkup Text
instance ToMarkup ResourceType where
    toMarkup = toMarkup . descResourceType
    preEscapedToMarkup = toMarkup . descResourceType

-- Describe a resource type in a short sentence.
descResourceType :: ResourceType -> Text
descResourceType BlogPost      = "Blog post"
descResourceType ForumPost     = "Forum post (e.g. Reddit comment)"
descResourceType LectureNotes  = "Lecture slides/notes"
descResourceType QAWebsite     = "Q&A website (e.g. Stack Overflow)"
descResourceType ResearchPaper = "Research paper"
descResourceType SourceCode    = "Source code (entire project)"
descResourceType Textbook      = "Textbook"
descResourceType VideoLecture  = "Video lecture/presentation"
