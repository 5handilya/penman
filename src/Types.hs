{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Types where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Frontmatter = Frontmatter
    {   title   :: !Text
    } deriving (Show, Generic)

instance FromJSON Frontmatter

data Document = Document
    {   docPart         :: !Int         -- Part number, parsed from frontmatter
    ,   docChapter      :: !Int         -- Chapter number, parsed from frontmatter
    ,   docTitle        :: !Text        -- Title, parsed from frontmatter
    ,   docSourcePath   :: !FilePath    -- Original path of the .md file
    ,   docOutputPath   :: !FilePath    -- Target path of the .html file
    ,   docContent      :: !Text        -- Raw Markdown content (excluding frontmatter)
    } deriving (Show)
