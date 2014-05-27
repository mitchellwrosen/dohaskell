module Model.ResourceType where

import Prelude

import Database.Persist.TH (derivePersistField)
import Data.Text           (Text)
import Text.Blaze          (ToMarkup, preEscapedToMarkup, toMarkup)

data ResourceType 
    = BlogPost
    | ForumPost
    | LectureNotes
    | ResearchPaper
    | QAWebsite
    | VideoLecture
    deriving (Eq, Read, Show)
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
descResourceType ResearchPaper = "Research paper"
descResourceType QAWebsite     = "Q&A website (e.g. Stack Overflow)"
descResourceType VideoLecture  = "Video lecture/presentation"
