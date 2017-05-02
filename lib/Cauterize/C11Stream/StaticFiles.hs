{-# LANGUAGE TemplateHaskell, CPP #-}
module Cauterize.C11Stream.StaticFiles
       ( allFiles
       ) where

#define EMBED_STATIC(FILE) \
  (FILE, $(embedFile ("static/" ++ FILE)))

import Data.FileEmbed
import qualified Data.ByteString as B

allFiles :: [(FilePath, B.ByteString)]
allFiles =
  [ EMBED_STATIC("cauterize.c")
  , EMBED_STATIC("cauterize.h")
  , EMBED_STATIC("cauterize_decode.c")
  , EMBED_STATIC("cauterize_decode.h")
  , EMBED_STATIC("cauterize_descriptors.c")
  , EMBED_STATIC("cauterize_descriptors.h")
  , EMBED_STATIC("cauterize_info.h")
  , EMBED_STATIC("cauterize_encode.c")
  , EMBED_STATIC("cauterize_encode.h")
  , EMBED_STATIC("cauterize_size.c")
  , EMBED_STATIC("cauterize_size.h")
  , EMBED_STATIC("cauterize_iterators.c")
  , EMBED_STATIC("cauterize_iterators.h")
  , EMBED_STATIC("cauterize_types.h")
  , EMBED_STATIC("cauterize_util.c")
  , EMBED_STATIC("cauterize_util.h")
  , EMBED_STATIC("crucible_interface.h")
  , EMBED_STATIC("crucible_main.c")
  ]
