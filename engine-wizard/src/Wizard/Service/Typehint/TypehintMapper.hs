module Wizard.Service.Typehint.TypehintMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Integration.Resource.Typehint.TypehintIDTO

toDTO :: String -> TypehintIDTO -> TypehintDTO
toDTO url iDto = TypehintDTO {_intId = iDto ^. intId, _name = iDto ^. name, _url = url}
