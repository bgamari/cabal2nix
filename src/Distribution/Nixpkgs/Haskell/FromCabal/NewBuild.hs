{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nixpkgs.Haskell.FromCabal.NewBuild where

import Data.List
import Data.Maybe
import Data.Function

import qualified Data.Map as Map
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Control.Lens
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(..), text, sep, vcat, hcat, hsep, nest )

import Distribution.Text
import Distribution.Package ( PackageName, packageName, packageId, UnitId )
import Distribution.Version
import Distribution.Compiler
import Distribution.System
import Distribution.Types.GenericPackageDescription (FlagName, mkFlagName)

import qualified Language.Nix as Nix
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Name
import qualified Distribution.Nixpkgs.Haskell.Derivation as D
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Fetch

newtype CachedPlan = CachedPlan { installPlan :: [PlanComponent] }

instance FromJSON CachedPlan where
    parseJSON = withObject "cached plan" $ \obj ->
      CachedPlan <$> obj .: "install-plan"

data PlanComponent = PlanComponent { pcType :: ComponentType
                                   , pcInstalledUnitId :: UnitId
                                   , pcPkgName :: PackageName
                                   , pcPkgVersion :: Version
                                   , pcDepends :: Maybe [UnitId]
                                   , pcStyle :: Maybe ComponentStyle
                                   , pcFlags :: Maybe [(FlagName, Bool)]
                                   }

instance FromJSON PlanComponent where
    parseJSON = withObject "plan component" $ \obj ->
      PlanComponent <$> obj .: "type"
                    <*> fromText obj "id"
                    <*> fromText obj "pkg-name"
                    <*> fromText obj "pkg-version"
                    <*> (obj .:? "depends" >>= mapM (mapM parseText))
                    <*> obj .:? "style"
                    <*> (obj .:? "flags" >>= pure . fmap (map flag . Map.toList))
      where
        fromText obj fld = do
            s <- obj .: fld
            parseText s
        parseText s = maybe (fail "uh oh") pure $ simpleParse s
        flag (k,v) = (mkFlagName k, v)

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

packagesFromProject :: IO [(Package, PlanComponent)]
packagesFromProject = do
    hackageDB <- loadHackageDB Nothing Nothing
    eitherPlan <- Aeson.eitherDecode <$> BSL.readFile "dist-newstyle/cache/plan.json"
    CachedPlan plan <- case eitherPlan of
      Left err -> fail $ show err
      Right x -> pure x
    killDups . catMaybes <$> mapM (packageFromComponent hackageDB) plan
  where
    killDups = nubBy ((==) `on` (packageId . fst)) . sortBy (compare `on` (packageId . fst))

packageFromComponent :: DB.HackageDB -> PlanComponent -> IO (Maybe (Package, PlanComponent))
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
    in do pkg <- getPackage' False hackageDB src
          pure $ Just (pkg, pc)

derivsFromProject :: IO [D.Derivation]
derivsFromProject = do
    pkgs <- packagesFromProject
    return [ packageToDeriv pkg pc
           | (pkg, pc) <- pkgs
           ]

packageToDeriv :: Package -> PlanComponent -> D.Derivation
packageToDeriv pkg pc =
    fromGenericPackageDescription (const True)
        (\i -> Just $ Nix.binding # (i, Nix.path # [i]))
        buildPlatform
        (unknownCompilerInfo buildCompilerId NoAbiTag)
        (maybe [] id $ pcFlags pc)
        []
        (pkgCabal pkg)
    & D.src .~ pkgSource pkg
    & D.doCheck .~ False -- FIXME

mkSet :: [(Nix.Identifier, Doc)] -> Doc
mkSet attrs = sep
     $ [ text "{" ]
    ++ map child attrs
    ++ [text "}"]
  where
    child (k,v) =
        nest 2 $ hcat [ views Nix.ident text k
                      , text " = "
                      , v
                      , text ";"
                      ]

mkLet :: [(Nix.Identifier, Doc)] -> Doc -> Doc
mkLet binds rhs = vcat $
    [ text "let"
    , nest 2 (sep $ map bindDoc binds)
    , text "in"
    , rhs
    ]
  where
    bindDoc (k,v) =
        hsep [ k ^. Nix.ident . to text
             , text "="
             , v
             , text ";"
             ]

formatDerivs :: [(Package, PlanComponent)] -> Doc
formatDerivs inputs = vcat
    [ text "args@{ stdenv, callPackage, ... }:"
    , text ""
    , text "with args;"
    , mkLet
        [ ("allPackages", mkSet allPackages)
        , ("instantiated", hsep [text "rec", mkSet instantiatedPackages])
        ]
        (text "instantiated")
    ]
  where
    allPackages =
        [ (d ^. D.pkgid . to toVersionedNixName, pPrint d)
        | (pkg, pc) <- inputs
        , let d = packageToDeriv pkg pc
        ]

    unitMap :: Map.Map UnitId Package
    unitMap = Map.fromList
        [ (pcInstalledUnitId pc, pkg)
        | (pkg, pc) <- inputs
        ]

    instantiatedPackages =
        [ (name, rhs)
        | (pkg, pc) <- inputs
        , let name = pkg ^. to packageId . to toVersionedNixName
              rhs = hsep
                  [ text "callPackage"
                  , ["allPackages", pkg ^. to packageId . to toVersionedNixName] ^. from Nix.path . to disp
                  , mkSet [ ( depPkg ^. to packageName . to toNixName
                            , depPkgIdent ^. to disp
                            )
                          | Just deps <- pure $ pcDepends pc
                          , dep <- deps
                          , Just depPkg <- pure $ Map.lookup dep unitMap
                          , let depPkgIdent = depPkg ^. to packageId . to toVersionedNixName
                          ]
                  ]
        ]

catPaths :: Nix.Path -> Nix.Path -> Nix.Path
catPaths p1 p2 =
    let p' = (p1 ^. Nix.path) ++ (p2 ^. Nix.path)
    in p' ^. from Nix.path
