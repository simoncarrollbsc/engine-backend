module Wizard.Service.BookReference.BookReferenceMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.BookReference.BookReferenceDTO
import Wizard.Model.BookReference.BookReference

toDTO :: BookReference -> BookReferenceDTO
toDTO br =
  BookReferenceDTO
    { _shortUuid = br ^. shortUuid
    , _bookChapter = br ^. bookChapter
    , _content = br ^. content
    , _createdAt = br ^. createdAt
    , _updatedAt = br ^. updatedAt
    }

fromDTO :: BookReferenceDTO -> BookReference
fromDTO dto =
  BookReference
    { _shortUuid = dto ^. shortUuid
    , _bookChapter = dto ^. bookChapter
    , _content = dto ^. content
    , _createdAt = dto ^. createdAt
    , _updatedAt = dto ^. updatedAt
    }
