module Cauterize.C11Stream.Options
  ( runWithOptions
  , CautC11Command(..)
  , CautC11GenOpts(..)
  , CautC11LibOpts(..)
  ) where

import Options.Applicative
import Cauterize.C11Stream.Version

runWithOptions :: (CautC11Command -> IO ()) -> IO ()
runWithOptions fn = do
  mopts <- execParser options
  case mopts of
    Just opts -> fn opts
    Nothing -> do
      putStr versionString
      putStrLn "dependencies:"
      putStr dependencyString

data CautC11Command
  = CautC11CommandGen CautC11GenOpts
  | CautC11CommandLib CautC11LibOpts
  | CautC11CommandAll CautC11GenOpts  CautC11LibOpts
  deriving (Show)

data CautC11GenOpts = CautC11GenOpts
  { specFile :: FilePath
  , genOutputDirectory :: FilePath
  } deriving (Show)

data CautC11LibOpts = CautC11LibOpts
  { libOutputDirectory :: FilePath
  } deriving (Show)

cautC11CommandAll :: CautC11GenOpts -> CautC11Command
cautC11CommandAll o = CautC11CommandAll o
  CautC11LibOpts { libOutputDirectory = genOutputDirectory o }

options :: ParserInfo (Maybe CautC11Command)
options = info (helper <*> o)
   ( fullDesc
  <> progDesc "Generate C11 code implementing Cauterize specification"
   )
  where
  o = flag' Nothing (long "version" <> hidden)
   <|> (Just <$> commandParser)

commandParser :: Parser CautC11Command
commandParser = subparser
  ( command "gen" (info (CautC11CommandGen <$> (helper <*> genOpts))
    (progDesc "Generate C11 code implementing Cauterize specification" ))
 <> command "lib" (info (CautC11CommandLib <$> (helper <*> libOpts))
    (progDesc "Generate C11 support library"))
 <> command "genall" (info (cautC11CommandAll <$> (helper <*> genOpts))
    (progDesc ("Generate C11 code implementing Cauterize specification, "
            ++ "and supporting library code")))
  )


genOpts :: Parser CautC11GenOpts
genOpts = CautC11GenOpts
  <$> argument str
    (  metavar "SPEC"
    <> help "Cauterize specification"
    )
  <*> argument str
    (  metavar "OUTDIR"
    <> help "Output directory"
    )

libOpts :: Parser CautC11LibOpts
libOpts = CautC11LibOpts
  <$> argument str
    (  metavar "OUTDIR"
    <> help "Output directory"
    )

