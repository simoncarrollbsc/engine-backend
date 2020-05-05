module Shared.Database.Migration.Development.KnowledgeModel.Data.References where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel

km1_ch1_q2_r1 :: Reference
km1_ch1_q2_r1 =
  ResourcePageReference
    { _uuid = fromJust $ U.fromString "903d0f50-040c-420e-9a65-49ba20ec6493"
    , _shortUuid = "bvq"
    }

km1_ch1_q2_r1Edited :: Reference
km1_ch1_q2_r1Edited =
  ResourcePageReference {_uuid = km1_ch1_q2_r1 ^. uuid, _shortUuid = "bbb"}

km1_ch1_q2_r1WithNewType :: Reference
km1_ch1_q2_r1WithNewType =
  URLReference
    { _uuid = km1_ch1_q2_r1 ^. uuid
    , _url = "https://wizard.org/dmp"
    , _label = "DMP Guide"
    }

km1_ch2_q6_r1 :: Reference
km1_ch2_q6_r1 =
  ResourcePageReference
    { _uuid = fromJust $ U.fromString "832ed9f5-107c-46e4-a13b-bf68086fcba1"
    , _shortUuid = "bvq"
    }

-- ---------------------------------------------------------------------------
km1_ch1_q2_r2 :: Reference
km1_ch1_q2_r2 =
  URLReference
    { _uuid = fromJust $ U.fromString "fc379161-540e-47fb-8547-0504d4a397bf"
    , _url = "https://wizard.org/fair"
    , _label = "F.A.I.R Principles"
    }

km1_ch1_q2_r2Edited :: Reference
km1_ch1_q2_r2Edited =
  URLReference
    { _uuid = km1_ch1_q2_r2 ^. uuid
    , _url = "EDITED: " ++ km1_ch1_q2_r2 ^. url
    , _label = "EDITED: " ++ km1_ch1_q2_r2 ^. label
    }

km1_ch1_q2_r2WithNewType :: Reference
km1_ch1_q2_r2WithNewType =
  CrossReference
    { _uuid = km1_ch1_q2_r2 ^. uuid
    , _targetUuid = fromJust $ U.fromString "9d109b01-ca61-4a6b-9906-22ad4ffc057b"
    , _description = "Link to my target"
    }

km1_ch2_q6_r2 :: Reference
km1_ch2_q6_r2 =
  URLReference
    { _uuid = fromJust $ U.fromString "29f973c8-1ec0-474a-8be5-84814c001496"
    , _url = "https://wizard.org/fair"
    , _label = "F.A.I.R Principles"
    }

-- ---------------------------------------------------------------------------
km1_ch1_q2_r3 :: Reference
km1_ch1_q2_r3 =
  CrossReference
    { _uuid = fromJust $ U.fromString "d032ac2e-f58b-4c4b-87a4-8fbd45f155fa"
    , _targetUuid = fromJust $ U.fromString "4ced8015-82ae-4cf9-952d-9730a84a825a"
    , _description = "Some description"
    }

km1_ch1_q2_r3Edited :: Reference
km1_ch1_q2_r3Edited =
  CrossReference
    { _uuid = km1_ch1_q2_r3 ^. uuid
    , _targetUuid = fromJust $ U.fromString "bfe0a3bc-ee9f-45b7-98a7-7462cf0dd914"
    , _description = "EDITED: " ++ km1_ch1_q2_r3 ^. description
    }

km1_ch1_q2_r3WithNewType :: Reference
km1_ch1_q2_r3WithNewType =
  ResourcePageReference {_uuid = km1_ch1_q2_r3 ^. uuid, _shortUuid = "awp"}

-- ---------------------------------------------------------------------------
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1 :: Reference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1 =
  ResourcePageReference
    { _uuid = fromJust $ U.fromString "994c2c75-4305-49bf-b207-c0d6f8042eb2"
    , _shortUuid = "bvq"
    }

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2 :: Reference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2 =
  URLReference
    { _uuid = fromJust $ U.fromString "931caf0b-a6ce-4183-8a02-7b02c2ff1e6c"
    , _url = "https://wizard.org/fair"
    , _label = "F.A.I.R Principles"
    }
