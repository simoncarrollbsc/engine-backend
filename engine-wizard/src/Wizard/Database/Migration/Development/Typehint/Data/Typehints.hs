module Wizard.Database.Migration.Development.Typehint.Data.Typehints where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO

lifeScienceTypehint :: TypehintDTO
lifeScienceTypehint =
  TypehintDTO {_intId = "op-p000001", _name = "Life Science Ontology", _url = "https://example.com/ontologies/${id}"}

mathematicalTypehint :: TypehintDTO
mathematicalTypehint =
  TypehintDTO {_intId = "op-p000008", _name = "Mathematical Ontology", _url = "https://example.com/ontologies/${id}"}

legalTypehint :: TypehintDTO
legalTypehint =
  TypehintDTO {_intId = "op-p000015", _name = "Legal Ontology", _url = "https://example.com/ontologies/${id}"}

typehintRequest :: TypehintRequestDTO
typehintRequest =
  TypehintRequestDTO
    { _packageId = Just $ germanyPackage ^. pId
    , _events = []
    , _questionUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _q = "dog"
    }
