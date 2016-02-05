module Main where

import Cauterize.C11Stream.Options
import Cauterize.C11Stream.HFile
import Cauterize.C11Stream.HTypes
import Cauterize.C11Stream.HDescriptors
import Cauterize.C11Stream.CDescriptors
import Cauterize.C11Stream.HInfo
import Cauterize.C11Stream.HInfoDefines
import Cauterize.C11Stream.CInfo
import Cauterize.C11Stream.StaticFiles
import Cauterize.C11Stream.CrucibleInterface
import Control.Monad
import Data.Text (unpack)
import Data.Maybe (isNothing, fromJust)
import System.Directory
import System.FilePath.Posix
import System.Exit
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B

main :: IO ()
main = runWithOptions caut2c11

caut2c11 :: Caut2C11Opts -> IO ()
caut2c11 (Caut2C11Opts { specFile = sf, outputDirectory = od, noLib = l, noGen = g }) = createGuard od $ do
  when (not g && isNothing sf) $ do
    putStrLn "Unless (--nogen|-G) is specified, a specification file must be provided (--spec|-s)."
    exitFailure

  when (not g) $ do
    spec <- loadSpec (fromJust sf)
    let baseName = unpack $ S.specName spec
    generateDynamicFiles od baseName spec
  when (not l) $ generateStaticFiles od

  where
    loadSpec :: FilePath -> IO S.Specification
    loadSpec p = do
      s <- S.parseSpecificationFromFile p
      case s of
        Left e -> error $ show e
        Right s' -> return s'

createGuard :: FilePath -> IO a -> IO a
createGuard out go = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else if de
          then go
          else createDirectory out >> go

generateStaticFiles :: FilePath -> IO ()
generateStaticFiles path = mapM_ staticWrite allFiles
  where
    staticWrite (p,c) = B.writeFile (path `combine` p) c

generateDynamicFiles :: FilePath -> String -> S.Specification -> IO ()
generateDynamicFiles path baseName spec = do
  writeFile (path `combine` (baseName ++ ".h")) (hFileFromSpec spec)
  writeFile (path `combine` (baseName ++ "_types.h")) (hTypesFromSpec spec)
  writeFile (path `combine` (baseName ++ "_descriptors.h")) (hDescriptorsFromSpec spec)
  writeFile (path `combine` (baseName ++ "_descriptors.c")) (cDescriptorsFromSpec spec)
  writeFile (path `combine` (baseName ++ "_infodefines.h")) (hInfoDefinesFromSpec spec)
  writeFile (path `combine` (baseName ++ "_info.h")) (hInfoFromSpec spec)
  writeFile (path `combine` (baseName ++ "_info.c")) (cInfoFromSpec spec)
  writeFile (path `combine` (baseName ++ "_crucible.c")) (crucibleFromSpec spec)
