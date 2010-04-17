#!/usr/bin/env runhaskell
import Distribution.Simple
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath.Posix

make = "ghc --make -fglasgow-exts -odir dist/build -hidir dist/build -idist/build:src Tests.hs -o tests -Ldist/build -lHSzoneinfo-0.5"

tests :: IO String
tests = do
    cd <- getCurrentDirectory
    return $ joinPath $ (++) (splitPath cd) ["tests"]

main :: IO ()
main =
    defaultMainWithHooks (autoconfUserHooks { runTests = quickCheck })
  where
    quickCheck _ _ _ _ = do
        status <- system make
        case status of
          ExitSuccess -> tests >>= system >> return ()
          _ -> return ()
