import Distribution.Nixpkgs.Haskell.FromCabal.NewBuild

main :: IO ()
main = do
    pkgs <- packagesFromProject
    writeFile "project.nix" $ show $ formatDerivs pkgs
    putStrLn $ "Derivations for "++show (length pkgs)++" packages written to project.nix"

