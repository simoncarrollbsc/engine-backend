module Wizard.Database.BSON.Config.AppConfig where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Config.SimpleFeature ()
import Wizard.Model.Config.AppConfig

instance ToBSON AppConfig where
  toBSON = toBSON'

instance FromBSON AppConfig where
  fromBSON = fromBSON'

instance ToBSON AppConfigOrganization where
  toBSON = toBSON'

instance FromBSON AppConfigOrganization where
  fromBSON = fromBSON'

instance ToBSON AppConfigAuth where
  toBSON = toBSON'

instance FromBSON AppConfigAuth where
  fromBSON = fromBSON'

instance ToBSON AppConfigAuthInternal where
  toBSON = toBSON'

instance FromBSON AppConfigAuthInternal where
  fromBSON = fromBSON'

instance ToBSON AppConfigAuthExternal where
  toBSON = toBSON'

instance FromBSON AppConfigAuthExternal where
  fromBSON = fromBSON'

instance ToBSON AppConfigAuthExternalService where
  toBSON = toBSON'

instance FromBSON AppConfigAuthExternalService where
  fromBSON = fromBSON'

instance ToBSON AppConfigAuthExternalServiceParameter where
  toBSON = toBSON'

instance FromBSON AppConfigAuthExternalServiceParameter where
  fromBSON = fromBSON'

instance ToBSON AppConfigAuthExternalServiceStyle where
  toBSON = toBSON'

instance FromBSON AppConfigAuthExternalServiceStyle where
  fromBSON = fromBSON'

instance ToBSON AppConfigPrivacyAndSupport where
  toBSON = toBSON'

instance FromBSON AppConfigPrivacyAndSupport where
  fromBSON = fromBSON'

instance ToBSON AppConfigDashboard where
  toBSON = toBSON'

instance FromBSON AppConfigDashboard where
  fromBSON = fromBSON'

instance ToBSON AppConfigDashboardWidgets where
  toBSON = toBSON'

instance FromBSON AppConfigDashboardWidgets where
  fromBSON = fromBSON'

instance ToBSON AppConfigLookAndFeel where
  toBSON = toBSON'

instance FromBSON AppConfigLookAndFeel where
  fromBSON = fromBSON'

instance ToBSON AppConfigLookAndFeelCustomMenuLink where
  toBSON = toBSON'

instance FromBSON AppConfigLookAndFeelCustomMenuLink where
  fromBSON = fromBSON'

instance ToBSON AppConfigRegistry where
  toBSON = toBSON'

instance FromBSON AppConfigRegistry where
  fromBSON = fromBSON'

instance ToBSON AppConfigQuestionnaire where
  toBSON = toBSON'

instance FromBSON AppConfigQuestionnaire where
  fromBSON = fromBSON'

instance ToBSON AppConfigQuestionnaireFeedback where
  toBSON = toBSON'

instance FromBSON AppConfigQuestionnaireFeedback where
  fromBSON = fromBSON'

instance ToBSON AppConfigTemplate where
  toBSON = toBSON'

instance FromBSON AppConfigTemplate where
  fromBSON = fromBSON'

instance ToBSON AppConfigSubmission where
  toBSON = toBSON'

instance FromBSON AppConfigSubmission where
  fromBSON = fromBSON'

instance ToBSON AppConfigSubmissionService where
  toBSON = toBSON'

instance FromBSON AppConfigSubmissionService where
  fromBSON = fromBSON'

instance ToBSON AppConfigSubmissionServiceSupportedFormat where
  toBSON = toBSON'

instance FromBSON AppConfigSubmissionServiceSupportedFormat where
  fromBSON = fromBSON'

instance ToBSON AppConfigSubmissionServiceRequest where
  toBSON = toBSON'

instance FromBSON AppConfigSubmissionServiceRequest where
  fromBSON = fromBSON'

instance ToBSON AppConfigSubmissionServiceRequestMultipart where
  toBSON = toBSON'

instance FromBSON AppConfigSubmissionServiceRequestMultipart where
  fromBSON = fromBSON'
