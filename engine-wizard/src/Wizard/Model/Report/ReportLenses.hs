module Wizard.Model.Report.ReportLenses where

import Wizard.Model.Report.Report

answeredQuestions' :: Functor f => (Int -> f Int) -> Indication -> f Indication
answeredQuestions' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Indication -> Int
    get entity@AnsweredIndication {} = _answeredIndicationAnsweredQuestions entity
    get entity@LevelsAnsweredIndication {} = _levelsAnsweredIndicationAnsweredQuestions entity
    set :: Indication -> Int -> Indication
    set entity@AnsweredIndication {} newValue = entity { _answeredIndicationAnsweredQuestions = newValue}
    set entity@LevelsAnsweredIndication {} newValue = entity { _levelsAnsweredIndicationAnsweredQuestions = newValue}

unansweredQuestions' :: Functor f => (Int -> f Int) -> Indication -> f Indication
unansweredQuestions' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Indication -> Int
    get entity@AnsweredIndication {} = _answeredIndicationUnansweredQuestions entity
    get entity@LevelsAnsweredIndication {} = _levelsAnsweredIndicationUnansweredQuestions entity
    set :: Indication -> Int -> Indication
    set entity@AnsweredIndication {} newValue = entity { _answeredIndicationUnansweredQuestions = newValue}
    set entity@LevelsAnsweredIndication {} newValue = entity { _levelsAnsweredIndicationUnansweredQuestions = newValue}

