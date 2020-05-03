module Wizard.Database.Migration.Development.Token.Data.Tokens where

import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Api.Resource.Token.TokenDTO

albertCreateToken :: TokenCreateDTO
albertCreateToken = TokenCreateDTO {_email = "albert.einstein@example.com", _password = "password"}

albertToken :: TokenDTO
albertToken = TokenDTO {_token = "abc"}
