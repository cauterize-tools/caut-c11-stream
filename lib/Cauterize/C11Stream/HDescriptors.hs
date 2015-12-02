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
            [ (C.primToText C.PBool, "primitive_type_id_bool" )
            , (C.primToText C.PF64 , "primitive_type_id_f64" )
            , (C.primToText C.PF32 , "primitive_type_id_f32" )
            , (C.primToText C.PS64 , "primitive_type_id_s64" )
            , (C.primToText C.PS32 , "primitive_type_id_s32" )
            , (C.primToText C.PS16 , "primitive_type_id_s16" )
            , (C.primToText C.PS8  , "primitive_type_id_s8" )
            , (C.primToText C.PU64 , "primitive_type_id_u64" )
            , (C.primToText C.PU32 , "primitive_type_id_u32" )
            , (C.primToText C.PU16 , "primitive_type_id_u16" )
            , (C.primToText C.PU8  , "primitive_type_id_u8" )
            ]
          fn (t, ix) = [i|    type_id_#{ln}_#{ident2str t} = #{ix},|]
      in intercalate "\n" $ map fn prims ++ map fn withIndex
