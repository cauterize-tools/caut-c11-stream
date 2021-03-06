{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.CInfo
       ( cInfoFromSpec
       ) where

import Cauterize.C11Stream.Util

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.List (intercalate)
import Data.Maybe

import qualified Cauterize.Specification as S

cInfoFromSpec :: S.Specification -> String
cInfoFromSpec s = unindent [i|
  #include "#{ln}_info.h"
  #include "#{ln}_types.h"

  #include "cauterize_info.h"

  #include <stdbool.h>
  #include <stdint.h>
  #include <stddef.h>

  #define ARR_ELEM_SPAN(TYPE) \\
    (uintptr_t)&(((TYPE *)NULL)[1])

#{fieldSets s types}

  struct type_info const type_info_#{ln}[TYPE_COUNT_#{ln}] = {
#{infoList}
  };

  struct schema_info const schema_info_#{ln} = {
    .name = "#{ln}",
    .fingerprint = SCHEMA_FP_#{ln},
    .min_size = SCHEMA_SIZE_MIN_#{ln},
    .max_size = SCHEMA_SIZE_MAX_#{ln},
    .depth = #{depth},
    .type_count = TYPE_COUNT_#{ln},
    .types = type_info_#{ln},
  };
|]
  where
    ln = specCName s
    types = S.specTypes s
    infoList = intercalate "\n" $ map info types
    depth = S.specDepth s

    -- Names to how you delcare that name
    info t =
      let n = ident2str ident
          ident = S.typeName t
          tdepth = S.typeDepth t
          tps = typeToPrimString t
          proto =
            case prototypeBody s t of
              Nothing -> [i|      /* No extra information for #{tps} #{n}. */|]
              Just s' -> chompNewline [i|
      .prototype.i_#{tps} = {
#{s'}
      },|]
      in chompNewline [i|
    {
      .name = "#{n}",
      .min_size = TYPE_SIZE_MIN_#{ln}_#{n},
      .max_size = TYPE_SIZE_MAX_#{ln}_#{n},
      .depth = #{tdepth},
      .fingerprint = TYPE_FP_#{ln}_#{n},
      .prototype_tag = caut_proto_#{tps},
#{proto}
    },|]

fieldSets :: S.Specification -> [S.Type] -> String
fieldSets s ts = intercalate "\n" $ mapMaybe (fieldSet s) ts

fieldSet :: S.Specification -> S.Type -> Maybe String
fieldSet s (S.Type { S.typeName = tn, S.typeDesc = d}) = n
  where
    tn' = ident2str tn
    n = case d of
      S.Synonym {}     -> Nothing
      S.Range {}       -> Nothing
      S.Array {}       -> Nothing
      S.Vector {}      -> Nothing
      S.Enumeration { S.enumerationValues = evs } -> Just $ mkValueSet tn' "enumeration" s evs
      S.Record { S.recordFields = fs} -> Just $ mkFieldSet tn' "record" s fs
      S.Combination { S.combinationFields = fs } -> Just $ mkFieldSet tn' "combination" s fs
      S.Union { S.unionFields = fs } -> Just $ mkFieldSet tn' "union" s fs

mkFieldSet :: String -> String -> S.Specification -> [S.Field] -> String
mkFieldSet name proto s fs = chompNewline [i|
  struct caut_field_info const #{proto}_field_infos_#{ln}_#{name}[] = {
#{intercalate "\n" $ map prototypeFieldInfo fs}
  };
|]
  where
    ln = specCName s

mkValueSet :: String -> String -> S.Specification -> [S.EnumVal] -> String
mkValueSet name proto s fs = chompNewline [i|
  struct caut_field_info const #{proto}_field_infos_#{ln}_#{name}[] = {
#{intercalate "\n" $ map ev fs}
  };
|]
  where
    ln = specCName s
    ev (S.EnumVal v ix) = [i|    { .name = "#{ident2str v}", .field_index = #{ix} },|]

prototypeFieldInfo :: S.Field -> String
prototypeFieldInfo S.EmptyField { S.fieldName = n, S.fieldIndex = ix }
  = chompNewline [i|
    { .name = "#{ident2str n}", .field_index = #{ix} },|]
prototypeFieldInfo S.DataField { S.fieldName = n, S.fieldIndex = ix }
  = chompNewline [i|
    { .name = "#{n'}", .field_index = #{ix}  },|]
  where
    n' = ident2str n

prototypeBody :: S.Specification -> S.Type -> Maybe String
prototypeBody s (S.Type { S.typeName = n, S.typeDesc = d }) =
  case d of
    S.Enumeration { S.enumerationValues = evs }
      -> Just $ chompNewline [i|
        .field_count = #{length evs}u,
        .fields = enumeration_field_infos_#{ln}_#{ident2str n},|]
    S.Record { S.recordFields = rs }
      -> Just $ chompNewline [i|
        .field_count = #{length rs}u,
        .fields = record_field_infos_#{ln}_#{ident2str n},|]
    S.Combination { S.combinationFields = cf }
      -> Just $ chompNewline [i|
        .field_count = #{length cf}u,
        .fields = combination_field_infos_#{ln}_#{ident2str n},|]
    S.Union { S.unionFields = uf }
      -> Just $ chompNewline [i|
        .field_count = #{length uf}u,
        .fields = union_field_infos_#{ln}_#{ident2str n},|]
    _ -> Nothing
  where
    ln = specCName s
