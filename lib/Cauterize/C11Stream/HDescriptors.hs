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
import qualified Cauterize.CommonTypes as C

hDescriptorsFromSpec :: S.Specification -> String
hDescriptorsFromSpec s = unindent [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include <stdint.h>

  #include "cauterize_descriptors.h"

  /* number of types */
  #define TYPE_COUNT_#{ln} (#{typeCount})

  /* schema depth */
  #define SCHEMA_DEPTH_#{ln} (#{depth})

  /* an index for each type */
#{typeIndiciesEnum}

  /* a descriptor for the entire schema */
  extern struct schema_descriptor const schema_descriptor_#{ln};

  /* an array of type descriptors */
  extern struct type_descriptor const type_descriptors_#{ln}[TYPE_COUNT_#{ln}];

  #endif /* #{guardSym} */
|]
  where
    guardSym = [i|_CAUTERIZE_C11STREAM_#{ln}_DESCRIPTORS_|]
    ln = unpack (S.specName s)
    depth = S.specDepth s
    types = S.specTypes s
    typeCount = length types

    typeIndiciesEnum = chompNewline [i|
  enum type_id_#{ln} {
#{typeIndicies}
  };
|]
    typeIndicies =
      let withIndex = zip (map S.typeName types) [(0 :: Integer)..]
          prims =
            [ (C.primToText C.PBool, (-11))
            , (C.primToText C.PF64 , (-10))
            , (C.primToText C.PF32 , ( -9))
            , (C.primToText C.PS64 , ( -8))
            , (C.primToText C.PS32 , ( -7))
            , (C.primToText C.PS16 , ( -6))
            , (C.primToText C.PS8  , ( -5))
            , (C.primToText C.PU64 , ( -4))
            , (C.primToText C.PU32 , ( -3))
            , (C.primToText C.PU16 , ( -2))
            , (C.primToText C.PU8  , ( -1))
            ]
          fn (t, ix) = [i|    type_id_#{ln}_#{ident2str t} = #{ix},|]
      in intercalate "\n" $ map fn (prims ++ withIndex)
