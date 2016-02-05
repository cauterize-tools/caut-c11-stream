module Cauterize.C11Stream.Options
  ( runWithOptions
  , Caut2C11Opts(..)
  ) where

import Options.Applicative

runWithOptions :: (Caut2C11Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

data Caut2C11Opts = Caut2C11Opts
  { specFile :: Maybe FilePath
  , outputDirectory :: FilePath
  , noLib :: Bool
  , noGen :: Bool
  } deriving (Show)

optParser :: Parser Caut2C11Opts
optParser = Caut2C11Opts
  <$> (optional $ strOption
    ( long "spec"
   <> short 's'
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    ) )
  <*> strOption
    ( long "output"
   <> short 'o'
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )
  <*> switch
    ( long "nolib"
   <> short 'L'
   <> help "Don't emit the Cauterize core library files."
    )
  <*> switch
    ( long "nogen"
   <> short 'G'
   <> help "Don't emit the generated C files."
    )

options :: ParserInfo Caut2C11Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )
