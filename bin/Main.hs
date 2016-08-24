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
import Cauterize.C11Stream.Util
import System.Directory
import System.FilePath.Posix
import qualified Cauterize.Specification as S
import qualified Data.ByteString as B

main :: IO ()
main = runWithOptions aux
  where aux (CautC11CommandGen opts) = genC11 opts
        aux (CautC11CommandLib opts) = genLib opts
        aux (CautC11CommandAll g l) = genC11 g >> genLib l

genC11 :: CautC11GenOpts -> IO ()
genC11 CautC11GenOpts { specFile = sf, genOutputDirectory = od} =
  createGuard od $ do
    spec <- loadSpec sf
    let baseName = specCName spec
    generateDynamicFiles od baseName spec
  where
    loadSpec :: FilePath -> IO S.Specification
    loadSpec p = do
      s <- S.parseSpecificationFromFile p
      case s of
        Left e -> error $ show e
        Right s' -> return s'

genLib :: CautC11LibOpts -> IO ()
genLib CautC11LibOpts { libOutputDirectory = od} =
  createGuard od $ generateStaticFiles od

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
