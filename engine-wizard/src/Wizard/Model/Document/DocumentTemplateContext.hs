module Wizard.Model.Document.DocumentTemplateContext where

import Data.Default
import Data.Text
import GHC.Generics

data DocumentTemplateContext =
  DocumentTemplateContext
    { _baseURL :: Text
    , _resourcePageURL :: Text
    }
  deriving (Show, Generic)

instance Default DocumentTemplateContext where
  def =
    DocumentTemplateContext
      { _baseURL = "https://ds-wizard.org"
      , _resourcePageURL = "https://researchers.ds-wizard.org/book-references/:shortuid"
      }
