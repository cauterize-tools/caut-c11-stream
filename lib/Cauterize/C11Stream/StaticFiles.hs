{-# LANGUAGE TemplateHaskell #-}
module Cauterize.C11Stream.StaticFiles
       ( allFiles
       ) where

import Data.FileEmbed
import qualified Data.ByteString as B

cauterizeDotH :: (FilePath, B.ByteString)
cauterizeDotH = ("cauterize.h", $(embedFile "static/cauterize.h"))

cauterizeDotC :: (FilePath, B.ByteString)
cauterizeDotC = ("cauterize.c", $(embedFile "static/cauterize.c"))

cauterizeTypesDotH :: (FilePath, B.ByteString)
cauterizeTypesDotH = ("cauterize_types.h", $(embedFile "static/cauterize_types.h"))

descriptorsDotH :: (FilePath, B.ByteString)
descriptorsDotH = ("cauterize_descriptors.h", $(embedFile "static/cauterize_descriptors.h"))

iteratorsDotH :: (FilePath, B.ByteString)
iteratorsDotH = ("cauterize_iterators.h", $(embedFile "static/cauterize_iterators.h"))

iteratorsDotC :: (FilePath, B.ByteString)
iteratorsDotC = ("cauterize_iterators.c", $(embedFile "static/cauterize_iterators.c"))

allFiles :: [(FilePath, B.ByteString)]
allFiles =
  [ cauterizeDotH
  , cauterizeDotC
  , cauterizeTypesDotH
  , descriptorsDotH
  , iteratorsDotH
  , iteratorsDotC
  ]
