module Model.ResourceType where

import Prelude

import Database.Persist.TH (derivePersistField)
import Data.Text           (Text)
import Text.Blaze          (ToMarkup, preEscapedToMarkup, toMarkup)

data ResourceType
    = BlogPost
    | CommunitySite
    | Dissertation
    | Documentation
    | ExperienceReport
    | ExtendedExample
    | ForumPost
    | FunctionalPearl
    | LectureNotes
    | MastersThesis
    | MetaResource
    | QAWebsite
    | ResearchPaper
    | ResearchPaperLite
    | SurveyArticle
    | Textbook
    | VideoLecture
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "ResourceType"

-- Reuse instance ToMarkup Text
instance ToMarkup ResourceType where
    toMarkup = toMarkup . descResourceType
    preEscapedToMarkup = toMarkup . descResourceType

-- Describe a resource type in a short sentence.
descResourceType :: ResourceType -> Text
descResourceType BlogPost          = "Blog post"
descResourceType CommunitySite     = "Community website"
descResourceType Dissertation      = "Dissertation"
descResourceType Documentation     = "Library documentation"
descResourceType ExperienceReport  = "Experience report"
descResourceType ExtendedExample   = "Extended example/tutorial"
descResourceType ForumPost         = "Forum post (e.g. Reddit comment)"
descResourceType FunctionalPearl   = "Functional pearl"
descResourceType LectureNotes      = "Lecture slides/notes"
descResourceType MastersThesis     = "Master's thesis"
descResourceType MetaResource      = "Meta-resource"
descResourceType QAWebsite         = "Q&A website"
descResourceType ResearchPaper     = "Research paper"
descResourceType ResearchPaperLite = "Research paper lite"
descResourceType SurveyArticle     = "Survey article"
descResourceType Textbook          = "Textbook"
descResourceType VideoLecture      = "Video lecture/presentation"

shortDescResourceType :: ResourceType -> Text
shortDescResourceType BlogPost          = "blog post"
shortDescResourceType CommunitySite     = "community website"
shortDescResourceType Dissertation      = "dissertation"
shortDescResourceType Documentation     = "documentation"
shortDescResourceType ExperienceReport  = "experience report"
shortDescResourceType ExtendedExample   = "tutorial"
shortDescResourceType ForumPost         = "forum post"
shortDescResourceType FunctionalPearl   = "functional pearl"
shortDescResourceType LectureNotes      = "lecture notes"
shortDescResourceType MastersThesis     = "thesis"
shortDescResourceType MetaResource      = "meta-resource"
shortDescResourceType QAWebsite         = "answer"
shortDescResourceType ResearchPaper     = "paper"
shortDescResourceType ResearchPaperLite = "paper lite"
shortDescResourceType SurveyArticle     = "survey"
shortDescResourceType Textbook          = "textbook"
shortDescResourceType VideoLecture      = "video"
