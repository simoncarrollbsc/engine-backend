module Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations where

import Control.Lens ((^.))
import Data.Map (fromList)
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel

ontologyPortal :: Integration
ontologyPortal =
  Integration
    { _uuid = fromJust $ U.fromString "e595a99e-5b10-4ac1-a6ef-379c849f9c84"
    , _iId = "ontologyPortal"
    , _name = "Ontology Portal"
    , _props = ["domain", "country"]
    , _logo = ""
    , _requestMethod = "GET"
    , _requestUrl = "${baseurl}/ontology-portal.json?domain=${domain}&country=${country}&q=${q}"
    , _requestHeaders = fromList [("Api-Key", "${apikey}")]
    , _requestBody = ""
    , _responseListField = "nested.results"
    , _responseIdField = "id"
    , _responseNameField = "name"
    , _itemUrl = "https://example.com/ontologies/${id}"
    }

ontologyPortalEdited :: Integration
ontologyPortalEdited =
  Integration
    { _uuid = ontologyPortal ^. uuid
    , _iId = "editedOntologyPortal"
    , _name = "EDITED: Ontology Portal"
    , _props = ["domain", "language"]
    , _logo = ""
    , _requestMethod = "PUT"
    , _requestUrl =
        "${baseurl}/ontology-portal-edited.json?domain=${domain}&language=${language}&q=${q}&edited"
    , _requestHeaders = fromList [("Api-Key-Edited", "${apikey}-EDITED")]
    , _requestBody = "{}"
    , _responseListField = "nested.results"
    , _responseIdField = "idEdited"
    , _responseNameField = "nameEdited"
    , _itemUrl = "https://example.com/ontologies-edited/${id}"
    }

bioPortal :: Integration
bioPortal =
  Integration
    { _uuid = fromJust $ U.fromString "32b5f11d-960b-4ce9-889f-fc7d29964122"
    , _iId = "bioPortal"
    , _name = "Bio Portal"
    , _props = ["domain", "branch"]
    , _logo = ""
    , _requestMethod = "GET"
    , _requestUrl = "${baseurl}/bio-portal.json?domain=${domain}&branch=${branch}&q=${q}"
    , _requestHeaders = fromList [("Api-Key", "${apikey}")]
    , _requestBody = ""
    , _responseListField = ""
    , _responseIdField = "id"
    , _responseNameField = "name"
    , _itemUrl = "https://example.com/portals/${id}"
    }
