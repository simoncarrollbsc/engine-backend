module Shared.LensesClasses where

class HasUuid' entity fieldType where
  uuid' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasChapterUuids' entity fieldType where
  chapterUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasTagUuids' entity fieldType where
  tagUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasQuestionUuids' entity fieldType where
  questionUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasIntegrationUuids' entity fieldType where
  integrationUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasExpertUuids' entity fieldType where
  expertUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasReferenceUuids' entity fieldType where
  referenceUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasAnswerUuids' entity fieldType where
  answerUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasItemTemplateQuestionUuids' entity fieldType where
  itemTemplateQuestionUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasFollowUpsUuids' entity fieldType where
  followUpUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasShortUuid' entity fieldType where
  shortUuid' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasUrl' entity fieldType where
  url' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasLabel' entity fieldType where
  label' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasTargetUuid' entity fieldType where
  targetUuid' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasDescription' entity fieldType where
  description' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity
  
  