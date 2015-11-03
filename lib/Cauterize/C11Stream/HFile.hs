{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.HFile
       ( hFileFromSpec
       ) where

import qualified Cauterize.Specification as S

import Data.Text (unpack)
import Data.String.Interpolate
import Data.String.Interpolate.Util

hFileFromSpec :: S.Specification -> String
hFileFromSpec s = unindent [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include "#{ln}_types.h"
  #include "#{ln}_descriptors.h"
  #include "#{ln}_encode.h"
  #include "#{ln}_decode.h"

  #endif /* #{guardSym} */
|]
  where
    guardSym = [i|_CAUTERIZE_C11STREAM_#{ln}_|]
    ln = unpack (S.specName s)
