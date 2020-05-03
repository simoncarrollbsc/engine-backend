module Wizard.Service.Statistics.StatisticsService where

import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics

getInstanceStatistics :: AppContextM InstanceStatistics
getInstanceStatistics = do
  uCount <- countUsers
  pCount <- countPackages
  qCount <- countQuestionnaires
  return InstanceStatistics {_userCount = uCount, _pkgCount = pCount, _qtnCount = qCount}
