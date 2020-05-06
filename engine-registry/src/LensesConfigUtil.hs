module LensesConfigUtil where

import Control.Lens hiding (makeFieldsNoPrefix)

--makeFieldsNoPrefix :: Name -> DecsQ
makeFieldsNoPrefix = makeLensesWith ((defaultFieldRules & lensField .~ classUnderscoreNoPrefixNamer))

--defaultFieldRules :: LensRules
--defaultFieldRules = LensRules
--  { _simpleLenses    = True
--  , _generateSigs    = True
--  , _generateClasses = True  -- classes will still be skipped if they already exist
--  , _allowIsos       = False -- generating Isos would hinder field class reuse
--  , _allowUpdates    = True
--  , _lazyPatterns    = False
--  , _classyLenses    = const Nothing
--  , _fieldToDef      = camelCaseNamer
--  }