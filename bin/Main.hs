module Main where

import Cauterize.C11Stream.Options
import Cauterize.C11Stream.HFile
import Cauterize.C11Stream.HTypes
import Cauterize.C11Stream.HDescriptors
import Cauterize.C11Stream.CDescriptors
import Data.Text (unpack)
import System.Directory
import System.FilePath.Posix
import qualified Cauterize.Specification as S

main :: IO ()
main = runWithOptions caut2c11

caut2c11 :: Caut2C11Opts -> IO ()
caut2c11 (Caut2C11Opts { specFile = sf, outputDirectory = od }) = createGuard od $ do
  spec <- loadSpec
  let baseName = unpack $ S.specName spec

  generateDynamicFiles od baseName spec
  where
    loadSpec :: IO S.Specification
    loadSpec = do
      s <- S.parseSpecificationFromFile sf
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

generateDynamicFiles :: FilePath -> String -> S.Specification -> IO ()
generateDynamicFiles path baseName spec = do
  writeFile (path `combine` (baseName ++ ".h")) (hFileFromSpec spec)
  writeFile (path `combine` (baseName ++ "_types.h")) (hTypesFromSpec spec)
  writeFile (path `combine` (baseName ++ "_descriptors.h")) (hDescriptorsFromSpec spec)
  writeFile (path `combine` (baseName ++ "_descriptors.c")) (cDescriptorsFromSpec spec)
--  writeFile (path `combine` (baseName ++ ".c")) (cFileFromSpec spec)
--  writeFile (path `combine` (baseName ++ "_message.h")) (hMessageFileFromSpec spec)
--  writeFile (path `combine` (baseName ++ "_message.c")) (cMessageFileFromSpec spec)
--  writeFile (path `combine` "test_client.h")  (testClientFromSpec spec)
--  writeFile (path `combine` "Makefile")  (makefileFromSpec spec)
