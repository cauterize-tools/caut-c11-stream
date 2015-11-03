{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.HDescriptors
       ( hDescriptorsFromSpec
       ) where

import Cauterize.C11Stream.Util

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import Data.List (intercalate)

import qualified Cauterize.Specification as S

hDescriptorsFromSpec :: S.Specification -> String
hDescriptorsFromSpec s = unindent [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include <stdint.h>

  #include "cauterize/type_descriptor.h"

  /* number of types */
  #define TYPE_COUNT_#{ln} (#{typeCount})

  /* an index for each type */
#{typeIndiciesEnum}

  /* an array of type descriptors */
  extern struct type_descriptor type_descriptors[TYPE_COUNT_#{ln}];

  #endif /* #{guardSym} */
|]
  where
    guardSym = [i|_CAUTERIZE_C11STREAM_#{ln}_DESCRIPTORS_|]
    ln = unpack (S.specName s)
    types = S.specTypes s
    typeCount = length types

    typeIndiciesEnum = chompNewline [i|
  enum type_index_#{ln} {
#{typeIndicies}
  };
|]
    typeIndicies =
      let withIndex = zip [(0 :: Integer)..] types
      in intercalate "\n" $ map (\(ix,t) -> [i|    type_index_#{ln}_#{ident2str $ S.typeName t} = #{ix},|]) withIndex
