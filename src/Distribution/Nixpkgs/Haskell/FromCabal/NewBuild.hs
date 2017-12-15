{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.NewBuild where

import Data.List
import Data.Maybe
import Data.Function

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Control.Lens
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(..), text, vcat, hcat, nest )

import Distribution.Text
import Distribution.Package (PackageName, pkgName, packageId)
import Distribution.Version
import Distribution.Compiler
import Distribution.System

import qualified Language.Nix as Nix
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import Distribution.Nixpkgs.Haskell.FromCabal
import qualified Distribution.Nixpkgs.Haskell.Derivation as D
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Fetch

newtype CachedPlan = CachedPlan { installPlan :: [PlanComponent] }

instance FromJSON CachedPlan where
    parseJSON = withObject "cached plan" $ \obj ->
      CachedPlan <$> obj .: "install-plan"

data PlanComponent = PlanComponent { pcType :: ComponentType
                                   , pcPkgName :: PackageName
                                   , pcPkgVersion :: Version
                                   , pcStyle :: Maybe ComponentStyle
                                   }

instance FromJSON PlanComponent where
    parseJSON = withObject "plan component" $ \obj ->
      PlanComponent <$> obj .: "type"
                    <*> fromText obj "pkg-name"
                    <*> fromText obj "pkg-version"
                    <*> obj .:? "style"
      where
        fromText obj fld = do
            s <- obj .: fld
            maybe (fail "uh oh") pure $ simpleParse s

data ComponentType = PreExisting | Configured

instance FromJSON ComponentType where
    parseJSON = withText "component type" f
      where
        f "pre-existing" = pure PreExisting
        f "configured" = pure Configured
        f s = fail $ "Unknown component style: "++show s

data ComponentStyle = LocalComponent | GlobalComponent | InplaceComponent

instance FromJSON ComponentStyle where
    parseJSON = withText "component style" f
      where
        f "global" = pure GlobalComponent
        f "local" = pure LocalComponent
        f "inplace" = pure InplaceComponent
        f s = fail $ "Unknown component style: "++show s

packagesFromProject :: IO [Package]
packagesFromProject = do
    hackageDB <- loadHackageDB Nothing Nothing
    eitherPlan <- Aeson.eitherDecode <$> BSL.readFile "dist-newstyle/cache/plan.json"
    CachedPlan plan <- case eitherPlan of
      Left err -> fail $ show err
      Right x -> pure x
    killDups . catMaybes <$> mapM (packageFromComponent hackageDB) plan
  where
    killDups = nubBy ((==) `on` packageId) . sortBy (compare `on` packageId)

packageFromComponent :: DB.HackageDB -> PlanComponent -> IO (Maybe Package)
packageFromComponent hackageDB pc
  | Just LocalComponent <- pcStyle pc = return Nothing
  | Just InplaceComponent <- pcStyle pc = return Nothing
  | PreExisting <- pcType pc = return Nothing
  | otherwise =
    let url = "cabal://" ++ display (pcPkgName pc) ++ "-" ++ display (pcPkgVersion pc)
        src = Source { sourceUrl = url
                     , sourceRevision = ""
                     , sourceHash = UnknownHash
                     , sourceCabalDir = ""
                     }
    in Just <$> getPackage' False hackageDB src

derivsFromProject :: IO [D.Derivation]
derivsFromProject = do
    pkgs <- packagesFromProject
    return $ map packageToDeriv pkgs

packageToDeriv :: Package -> D.Derivation
packageToDeriv pkg =
    fromGenericPackageDescription (const True)
        (\i -> Just $ Nix.binding # (i, Nix.path # [i]))
        buildPlatform
        (unknownCompilerInfo buildCompilerId NoAbiTag)
        []
        []
        (pkgCabal pkg)
    & D.src .~ pkgSource pkg

formatDerivs :: [D.Derivation] -> Doc
formatDerivs derivs = vcat
    $ [ text "args:"
      , text ""
      , text "with args;"
      , text "{"
      ]
    ++ map child derivs
    ++ [text "}"]
  where
    child d = nest 2 $ hcat [ text (d ^. D.pkgid . to (display . pkgName))
                            , text " = "
                            , pPrint d
                            , text ";"
                            ]
