module Shared.Database.Migration.Development.KnowledgeModel.Data.Experts where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel

km1_ch1_q2_eAlbert :: Expert
km1_ch1_q2_eAlbert =
  Expert
    { _uuid = fromJust $ U.fromString "04b3661a-4176-4d5e-954f-6827f1888b8f"
    , _name = "Albert Einstein"
    , _email = "albert.einstein@example.com"
    }

km1_ch1_q2_eAlbertEdited :: Expert
km1_ch1_q2_eAlbertEdited =
  Expert
    { _uuid = km1_ch1_q2_eAlbert ^. uuid
    , _name = "EDITED: Albert Einstein"
    , _email = "EDITED: albert.einstein@example.com"
    }

km1_ch1_q2_eNikola :: Expert
km1_ch1_q2_eNikola =
  Expert
    { _uuid = fromJust $ U.fromString "78597e39-628a-47fd-8b3d-cc149f1c53e9"
    , _name = "Nikola Tesla"
    , _email = "nikola.tesla@example.com"
    }

km1_ch1_q2_eIsaac :: Expert
km1_ch1_q2_eIsaac =
  Expert
    { _uuid = fromJust $ U.fromString "e56a5fea-6e01-4898-8db0-741200073752"
    , _name = "Isaac Newton"
    , _email = "isaac.newton@example.com"
    }

-- ---------------------------------------------------------------------------
km1_ch2_q6_eAlbert :: Expert
km1_ch2_q6_eAlbert =
  Expert
    { _uuid = fromJust $ U.fromString "6fbed760-a612-485f-8f0a-7d69b97a103a"
    , _name = "Albert Einstein"
    , _email = "albert.einstein@example.com"
    }

km1_ch2_q6_eNikola :: Expert
km1_ch2_q6_eNikola =
  Expert
    { _uuid = fromJust $ U.fromString "0d545857-179a-49e7-ac98-934b91a53e93"
    , _name = "Nikola Tesla"
    , _email = "nikola.tesla@example.com"
    }

-- ---------------------------------------------------------------------------
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eAlbert :: Expert
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eAlbert =
  Expert
    { _uuid = fromJust $ U.fromString "14c3db17-923a-4c1c-8cf0-8d5bad682b3f"
    , _name = "Albert Einstein"
    , _email = "albert.einstein@example.com"
    }

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola :: Expert
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola =
  Expert
    { _uuid = fromJust $ U.fromString "b8560122-a5ad-4742-9d8a-331b117cb831"
    , _name = "Nikola Tesla"
    , _email = "nikola.tesla@example.com"
    }
