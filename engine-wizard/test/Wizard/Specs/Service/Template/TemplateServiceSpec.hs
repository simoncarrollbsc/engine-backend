module Wizard.Specs.Service.Template.TemplateServiceSpec where

import qualified Data.UUID as U
import Test.Hspec

import Wizard.Model.Template.Template
import Wizard.Service.Template.TemplateService

templateServiceSpec =
  describe "Package Utils" $ do
    let pkgId = ["org.nl", "core-nl", "2.0.0"]
    describe "filterTemplates" $ do
      it "No KM Specifications given => Deny" $
            -- GIVEN:
       do
        let templates =
              [ Template
                  { _uuid = U.nil
                  , _name = ""
                  , _description = ""
                  , _allowedPackages = []
                  , _recommendedPackageId = Nothing
                  , _formats = []
                  }
              ]
            -- AND:
        let expectation = 0
            -- GIVEN:
        let result = filterTemplates pkgId templates
            -- THEN:
        length result `shouldBe` expectation
      it "One relevant KM Specifications given => Allow" $
            -- GIVEN:
       do
        let templates =
              [ Template
                  { _uuid = U.nil
                  , _name = ""
                  , _description = ""
                  , _allowedPackages =
                      [ TemplateAllowedPackage
                          {_orgId = Nothing, _kmId = Nothing, _minVersion = Nothing, _maxVersion = Nothing}
                      ]
                  , _recommendedPackageId = Nothing
                  , _formats = []
                  }
              ]
            -- AND:
        let expectation = 1
            -- GIVEN:
        let result = filterTemplates pkgId templates
            -- THEN:
        length result `shouldBe` expectation
      it "One relevant and one non-relevant KM Specifications given => Allow" $
            -- GIVEN:
       do
        let templates =
              [ Template
                  { _uuid = U.nil
                  , _name = ""
                  , _description = ""
                  , _allowedPackages =
                      [ TemplateAllowedPackage
                          {_orgId = Nothing, _kmId = Nothing, _minVersion = Nothing, _maxVersion = Nothing}
                      , TemplateAllowedPackage
                          {_orgId = Nothing, _kmId = Just "core-de", _minVersion = Nothing, _maxVersion = Nothing}
                      ]
                  , _recommendedPackageId = Nothing
                  , _formats = []
                  }
              ]
            -- AND:
        let expectation = 1
            -- GIVEN:
        let result = filterTemplates pkgId templates
            -- THEN:
        length result `shouldBe` expectation
    describe "fitsIntoKMSpec" $ do
      it "No restrictions => Allow anything" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage {_orgId = Nothing, _kmId = Nothing, _minVersion = Nothing, _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'orgId', same provided 'orgId' => Allow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Just "org.nl", _kmId = Nothing, _minVersion = Nothing, _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'orgId', different provided 'orgId' => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Just "org.de", _kmId = Nothing, _minVersion = Nothing, _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` False
      it "Restriction on 'KmID', same provided 'KmID' => Allow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Nothing, _kmId = Just "core-nl", _minVersion = Nothing, _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'KmID', different provided 'KmID' => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Nothing, _kmId = Just "core-de", _minVersion = Nothing, _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` False
      it "Restriction on 'minimal version', provided higher version => Alow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Nothing, _kmId = Nothing, _minVersion = Just "1.0.0", _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'minimal version', provided lower version => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Nothing, _kmId = Nothing, _minVersion = Just "2.0.1", _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` False
      it "Restriction on 'maximal version', provided lower version => Alow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Nothing, _kmId = Nothing, _minVersion = Nothing, _maxVersion = Just "2.0.1"}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'maximal version', provided higher version => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Nothing, _kmId = Nothing, _minVersion = Nothing, _maxVersion = Just "1.0.0"}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` False
      it "Two restrictions, provided data satisfies just one => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                {_orgId = Just "org.de", _kmId = Just "core-nl", _minVersion = Nothing, _maxVersion = Nothing}
        -- GIVEN:
        let result = fitsIntoKMSpec pkgId kmSpec
        -- THEN:
        result `shouldBe` False
