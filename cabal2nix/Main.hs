{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main ( main ) where

import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe
import Distribution.Compiler
import Distribution.ModuleName ( ModuleName )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text
import Distribution.Types.LegacyExeDependency
import Distribution.Types.PkgconfigDependency
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree
import Distribution.Verbosity
import Distribution.Version
import GHC.Generics ( Generic )
import Language.Haskell.Extension
import System.Environment
import System.IO.Unsafe
import Text.PrettyPrint
import Text.Show.Pretty

data Component = Lib | Exe String | Test String
  deriving (Show, Eq, Ord, Generic)

data Config = CabalConf ConfVar | Config Component
  deriving (Show, Eq, Generic)

type CTree = CondTree ConfVar [Dependency]

data IfThen a = (Condition Config) :=> a
  deriving (Show, Generic)

infixr 0 :=>
data BInfo = BInfo
  { disabled  :: Condition Config
  , haskell   :: [IfThen Dependency]
  , pkgconfig :: [IfThen PkgconfigDependency]
  , system    :: [IfThen Dependency]
  , tools     :: [IfThen LegacyExeDependency]
  }
  deriving (Show, Generic)

instance Monoid BInfo where
  mappend (BInfo x1 x2 x3 x4 x5) (BInfo y1 y2 y3 y4 y5) = BInfo (cOr x1 y1) (mappend x2 y2) (mappend x3 y3) (mappend x4 y4) (mappend x5 y5)
  mempty = BInfo (Lit False) mempty mempty mempty mempty

buildinfo2binfo :: Condition Config -> BuildInfo -> BInfo
buildinfo2binfo cond bi = BInfo
  { disabled = if buildable bi then mempty else cond
  , haskell =  map (cond :=>) (targetBuildDepends bi)
  , pkgconfig = map (cond :=>) (pkgconfigDepends bi)
  , system = [ cond :=> (Dependency (mkPackageName x) anyVersion) | x <- extraLibs bi ]
  , tools = map (cond :=>) (buildTools bi)
  }

library2binfo :: Condition Config -> CTree Library -> BInfo
library2binfo ctx (CondNode lib _ comps) = mconcat $
  [ buildinfo2binfo ctx (libBuildInfo lib) ] ++ map (libraryComponents2binfo ctx) comps
  where
    libraryComponents2binfo :: Condition Config -> (Condition ConfVar, CTree Library, Maybe (CTree Library)) -> BInfo
    libraryComponents2binfo ctx (cond,true,false) =
      library2binfo (cAnd ctx (cabalConf cond)) true `mappend` maybe mempty (library2binfo (cAnd ctx (cNot (cabalConf cond)))) false

executable2binfo :: Condition Config -> CTree Executable -> BInfo
executable2binfo ctx (CondNode exe _ comps) = mconcat $
  [ buildinfo2binfo ctx (buildInfo exe) ] ++ map (executableComponents2binfo ctx) comps
  where
    executableComponents2binfo :: Condition Config -> (Condition ConfVar, CTree Executable, Maybe (CTree Executable)) -> BInfo
    executableComponents2binfo ctx (cond,true,false) =
      executable2binfo (cAnd ctx (cabalConf cond)) true `mappend` maybe mempty (executable2binfo (cAnd ctx (cNot (cabalConf cond)))) false

test2binfo :: Condition Config -> CTree TestSuite -> BInfo
test2binfo ctx (CondNode test _ comps) = mconcat $
  [ buildinfo2binfo ctx (testBuildInfo test) ] ++ map (testComponents2binfo ctx) comps
  where
    testComponents2binfo :: Condition Config -> (Condition ConfVar, CTree TestSuite, Maybe (CTree TestSuite)) -> BInfo
    testComponents2binfo ctx (cond,true,false) =
      test2binfo (cAnd ctx (cabalConf cond)) true `mappend` maybe mempty (test2binfo (cAnd ctx (cNot (cabalConf cond)))) false

gdp2binfo :: GenericPackageDescription -> [(Component,BInfo)]
gdp2binfo gpd = maybe [] (return . (,) Lib . (library2binfo (Lit True))) (condLibrary gpd)
             ++ map (\(name,ctree) -> (Exe name, (executable2binfo (Lit True) ctree))) (condExecutables gpd)
             ++ map (\(name,ctree) -> (Test name, (test2binfo (Lit True) ctree))) (condTestSuites gpd)

cabalConf :: Condition ConfVar -> Condition Config
cabalConf (Var c)      = Var (CabalConf c)
cabalConf (Lit b)      = Lit b
cabalConf (CNot c)     = cNot (cabalConf c)
cabalConf (CAnd c1 c2) = cAnd (cabalConf c1) (cabalConf c2)
cabalConf (COr c1 c2)  = cOr (cabalConf c1) (cabalConf c2)

-------------------------------------------------------------------------------

gpd :: Monad m => String -> String -> m GenericPackageDescription
gpd ctx buf = case parsePackageDescription buf of
                ParseFailed perr -> fail (ctx ++ ": " ++ show perr)
                ParseOk _ x      -> return x

main :: IO ()
main = do [fpath] <- getArgs
          buf <- readFile fpath
          binfo <- gdp2binfo <$> gpd fpath buf
          putStrLn (dumpStr binfo)

instance PrettyVal Component where
instance PrettyVal BInfo where
instance PrettyVal Dependency where prettyVal = String . display
instance PrettyVal Config where
instance PrettyVal OS where
instance PrettyVal Arch where
instance PrettyVal FlagName where
instance PrettyVal CompilerFlavor where
instance PrettyVal VersionRange where
instance PrettyVal Version where

instance PrettyVal a => PrettyVal (IfThen a) where
  prettyVal (Lit True :=> body) = prettyVal body
  prettyVal (cond :=> body) = InfixCons (prettyVal cond) [(":=>", prettyVal body)]

instance PrettyVal (Condition ConfVar) where
  prettyVal = String . show . ppCondition

instance PrettyVal (Condition Config) where

ppCondition :: Condition ConfVar -> Doc
ppCondition (Var x)        = ppConfVar x
ppCondition (Lit b)        = text (show b)
ppCondition (CNot c)       = char '!' <> (ppCondition c)
ppCondition (COr c1 c2)    = parens (hsep [ppCondition c1, text "||" <+> ppCondition c2])
ppCondition (CAnd c1 c2)   = parens (hsep [ppCondition c1, text "&&" <+> ppCondition c2])

instance PrettyVal ConfVar where
  prettyVal = String . show . ppConfVar

ppConfVar :: ConfVar -> Doc
ppConfVar (OS os)          = text "os"   <> parens (disp os)
ppConfVar (Arch arch)      = text "arch" <> parens (disp arch)
ppConfVar (Flag name)      = text "flag" <> parens (ppFlagName name)
ppConfVar (Impl c v)       = text "impl" <> parens (disp c <+> disp v)

ppFlagName :: FlagName -> Doc
ppFlagName = text . unFlagName
