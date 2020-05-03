module Wizard.Database.Migration.Development.Template.Data.Templates where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Model.Template.Template

commonWizardTemplate =
  Template
    { _uuid = fromJust $ U.fromString "43a3fdd1-8535-42e0-81a7-5edbff296e65"
    , _name = "Default Template"
    , _description = "This is a default template"
    , _allowedPackages = [templateAllowedPackage]
    , _recommendedPackageId = Nothing
    , _formats =
        [ templateFormatJson
        , templateFormatHtml
        , templateFormatPdf
        , templateFormatLatex
        , templateFormatDocx
        , templateFormatOdt
        , templateFormatMarkdown
        ]
    }

templateAllowedPackage :: TemplateAllowedPackage
templateAllowedPackage =
  TemplateAllowedPackage {_orgId = Nothing, _kmId = Nothing, _minVersion = Nothing, _maxVersion = Nothing}

templateFormatJson :: TemplateFormat
templateFormatJson =
  TemplateFormat
    { _uuid = fromJust $ U.fromString "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
    , _name = "JSON Data"
    , _shortName = "json"
    , _icon = "far fa-file"
    , _color = "#f15a24"
    }

templateFormatHtml :: TemplateFormat
templateFormatHtml =
  TemplateFormat
    { _uuid = fromJust $ U.fromString "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
    , _name = "HTML Document"
    , _shortName = "html"
    , _icon = "far fa-file-code"
    , _color = "#f15a24"
    }

templateFormatPdf :: TemplateFormat
templateFormatPdf =
  TemplateFormat
    { _uuid = fromJust $ U.fromString "68c26e34-5e77-4e15-9bf7-06ff92582257"
    , _name = "PDF Document"
    , _shortName = "pdf"
    , _icon = "far fa-file-pdf"
    , _color = "#f15a24"
    }

templateFormatLatex :: TemplateFormat
templateFormatLatex =
  TemplateFormat
    { _uuid = fromJust $ U.fromString "dbc94579-40d7-42c3-975c-71e30d07778b"
    , _name = "LaTeX Document"
    , _shortName = "latex"
    , _icon = "far fa-file-alt"
    , _color = "#f15a24"
    }

templateFormatDocx :: TemplateFormat
templateFormatDocx =
  TemplateFormat
    { _uuid = fromJust $ U.fromString "f4bd941a-dfbe-4226-a1fc-200fb5269311"
    , _name = "MS Word Document"
    , _shortName = "docx"
    , _icon = "far fa-file-word"
    , _color = "#f15a24"
    }

templateFormatOdt :: TemplateFormat
templateFormatOdt =
  TemplateFormat
    { _uuid = fromJust $ U.fromString "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
    , _name = "OpenDocument Text"
    , _shortName = "odt"
    , _icon = "far fa-file-alt"
    , _color = "#f15a24"
    }

templateFormatMarkdown :: TemplateFormat
templateFormatMarkdown =
  TemplateFormat
    { _uuid = fromJust $ U.fromString "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"
    , _name = "Markdown Document"
    , _shortName = "md"
    , _icon = "far fa-file-alt"
    , _color = "#f15a24"
    }

commonWizardTemplateDTO :: TemplateDTO
commonWizardTemplateDTO =
  TemplateDTO
    { _uuid = commonWizardTemplate ^. uuid
    , _name = commonWizardTemplate ^. name
    , _description = commonWizardTemplate ^. description
    , _allowedPackages = []
    , _recommendedPackageId = Nothing
    , _formats =
        [ templateFormatJson
        , templateFormatHtml
        , templateFormatPdf
        , templateFormatLatex
        , templateFormatDocx
        , templateFormatOdt
        , templateFormatMarkdown
        ]
    }
