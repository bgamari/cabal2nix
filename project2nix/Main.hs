import Distribution.Nixpkgs.Haskell.FromCabal.NewBuild

main :: IO ()
main = do
    derivs <- derivsFromProject
    writeFile "project.nix" $ show $ formatDerivs derivs
    putStrLn $ "Derivations for "++show (length derivs)++" packages written to project.nix"

