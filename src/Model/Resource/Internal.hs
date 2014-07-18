module Model.Resource.Internal where

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
    | SourceCode
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
descResourceType Documentation     = "Documentation"
descResourceType ExperienceReport  = "Experience report"
descResourceType ExtendedExample   = "Extended example/tutorial"
descResourceType ForumPost         = "Forum post (e.g. Reddit comment)"
descResourceType FunctionalPearl   = "Functional pearl"
descResourceType LectureNotes      = "Lecture slides/notes"
descResourceType MastersThesis     = "Master's thesis"
descResourceType MetaResource      = "Meta-resource"
descResourceType QAWebsite         = "Q&A website"
descResourceType ResearchPaper     = "Research paper"
descResourceType ResearchPaperLite = "Light research paper"
descResourceType SourceCode        = "Source code"
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
shortDescResourceType ResearchPaperLite = "light paper"
shortDescResourceType SurveyArticle     = "survey"
shortDescResourceType SourceCode        = "source code"
shortDescResourceType Textbook          = "textbook"
shortDescResourceType VideoLecture      = "video"

-- | IMPORTANT: keep in sync with shortReadResourceTypePlural!
shortDescResourceTypePlural :: ResourceType -> Text
shortDescResourceTypePlural BlogPost          = "blog posts"
shortDescResourceTypePlural CommunitySite     = "community websites"
shortDescResourceTypePlural Dissertation      = "dissertations"
shortDescResourceTypePlural Documentation     = "documentations"
shortDescResourceTypePlural ExperienceReport  = "experience reports"
shortDescResourceTypePlural ExtendedExample   = "tutorials"
shortDescResourceTypePlural ForumPost         = "forum posts"
shortDescResourceTypePlural FunctionalPearl   = "functional pearls"
shortDescResourceTypePlural LectureNotes      = "lecture notes"
shortDescResourceTypePlural MastersThesis     = "theses"
shortDescResourceTypePlural MetaResource      = "meta-resources"
shortDescResourceTypePlural QAWebsite         = "answers"
shortDescResourceTypePlural ResearchPaper     = "papers"
shortDescResourceTypePlural ResearchPaperLite = "light papers"
shortDescResourceTypePlural SurveyArticle     = "surveys"
shortDescResourceTypePlural SourceCode        = "source c0dez"
shortDescResourceTypePlural Textbook          = "textbooks"
shortDescResourceTypePlural VideoLecture      = "videos"

-- | IMPORTANT: keep in sync with shortDescResourceTypePlural!
shortReadResourceTypePlural :: Text -> Maybe ResourceType
shortReadResourceTypePlural "blog posts"         = Just BlogPost
shortReadResourceTypePlural "community websites" = Just CommunitySite
shortReadResourceTypePlural "dissertations"      = Just Dissertation
shortReadResourceTypePlural "documentations"     = Just Documentation
shortReadResourceTypePlural "experience reports" = Just ExperienceReport
shortReadResourceTypePlural "tutorials"          = Just ExtendedExample
shortReadResourceTypePlural "forum posts"        = Just ForumPost
shortReadResourceTypePlural "functional pearls"  = Just FunctionalPearl
shortReadResourceTypePlural "lecture notes"      = Just LectureNotes
shortReadResourceTypePlural "theses"             = Just MastersThesis
shortReadResourceTypePlural "meta-resources"     = Just MetaResource
shortReadResourceTypePlural "answers"            = Just QAWebsite
shortReadResourceTypePlural "papers"             = Just ResearchPaper
shortReadResourceTypePlural "light papers"       = Just ResearchPaperLite
shortReadResourceTypePlural "surveys"            = Just SurveyArticle
shortReadResourceTypePlural "source c0dez"       = Just SourceCode
shortReadResourceTypePlural "textbooks"          = Just Textbook
shortReadResourceTypePlural "videos"             = Just VideoLecture
shortReadResourceTypePlural _                    = Nothing
