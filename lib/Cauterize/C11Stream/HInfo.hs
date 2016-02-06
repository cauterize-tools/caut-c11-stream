{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.HInfo
       ( hInfoFromSpec
       ) where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)

import qualified Cauterize.Specification as S

hInfoFromSpec :: S.Specification -> String
hInfoFromSpec s = unindent [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include <stdint.h>

  #include "cauterize_info.h"
  #include "#{ln}_descriptors.h"
  #include "#{ln}_infodefines.h"

  /* number of types */
  #define INFO_COUNT_#{ln} (#{typeCount})

  /* a descriptor for the entire schema */
  extern struct schema_info const schema_info_#{ln};

  #endif /* #{guardSym} */
|]
  where
    guardSym = [i|_CAUTERIZE_C11STREAM_#{ln}_INFOS_|]
    ln = unpack (S.specName s)
    types = S.specTypes s
    typeCount = length types
