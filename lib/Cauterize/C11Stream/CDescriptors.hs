{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.CDescriptors
       ( cDescriptorsFromSpec
       ) where

import Cauterize.C11Stream.Util

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import Data.List (intercalate)
import Data.Maybe

import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C

cDescriptorsFromSpec :: S.Specification -> String
cDescriptorsFromSpec s = unindent [i|
  #include "#{ln}_descriptors.h"

  #include <stdbool.h>
  #include <stdint.h>
  #include <stddef.h>

#{fieldSets s types}

  struct type_descriptor const type_descriptors[TYPE_COUNT_#{ln}] = {
#{descriptorList}
  };|]
  where
    ln = unpack (S.specName s)
    types = S.specTypes s
    descriptorList = intercalate "\n" $ map descriptor types

    descriptor t =
      let n = ident2str (S.typeName t)
          tps = typeToPrimString t
      in chompNewline [i|
    {
      .name = "#{n}",
      .type_id = type_id_#{ln}_#{n},
      .prototype_tag = caut_proto_#{tps},
      .prototype.c_#{tps} = {
#{prototypeBody s t}
      },
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
      S.Enumeration {} -> Nothing
      S.Record { S.recordFields = fs} -> Just $ mkFieldSet tn' "record" s fs
      S.Combination { S.combinationFields = fs } -> Just $ mkFieldSet tn' "combination" s fs
      S.Union { S.unionFields = fs } -> Just $ mkFieldSet tn' "union" s fs

mkFieldSet :: String -> String -> S.Specification -> [S.Field] -> String
mkFieldSet name proto s fs = chompNewline [i|
  struct caut_field const #{proto}_fields_#{ln}_#{name}[] = {
#{intercalate "\n" $ map (prototypeField s) fs}
  };
|]
  where
    ln = unpack (S.specName s)

typeToPrimString :: S.Type -> String
typeToPrimString S.Type { S.typeDesc = d } = n
  where
    n = case d of
      S.Synonym {}     -> "synonym"
      S.Range {}       -> "range"
      S.Array {}       -> "array"
      S.Vector {}      -> "vector"
      S.Enumeration {} -> "enumeration"
      S.Record {}      -> "record"
      S.Combination {} -> "combination"
      S.Union {}       -> "union"

prototypeField :: S.Specification -> S.Field -> String
prototypeField _ S.EmptyField { S.fieldName = n, S.fieldIndex = ix }
  = chompNewline [i|
    { .name = "#{ident2str n}", .field_index = #{ix}, .data = false, .ref_id = 0 },|]
prototypeField s S.DataField { S.fieldName = n, S.fieldIndex = ix, S.fieldRef = r }
  = chompNewline [i|
    { .name = "#{ident2str n}", .field_index = #{ix}, .data = true, .ref_id = type_id_#{ln}_#{ident2str r} },|]
  where
    ln = unpack (S.specName s)


prototypeBody :: S.Specification -> S.Type -> String
prototypeBody s (S.Type { S.typeName = n, S.typeDesc = d }) =
  case d of
    S.Synonym { S.synonymRef = r }
      -> chompNewline [i|
        .ref_id = type_id_#{ln}_#{ident2str r},|]
    S.Range { S.rangeOffset = ro, S.rangeLength = rl, S.rangeTag = rt }
      -> chompNewline [i|
        .offset = #{ro},
        .length = #{rl},
        .tag = #{tagToTagEnumStr rt},|]
    S.Array { S.arrayRef = r, S.arrayLength = al }
      -> chompNewline [i|
        .ref_id = type_id_#{ln}_#{ident2str r},
        .length = #{al},|]
    S.Vector { S.vectorRef = r, S.vectorLength = vl }
      -> chompNewline [i|
        .ref_id = type_id_#{ln}_#{ident2str r},
        .max_length = #{vl},|]
    S.Enumeration { S.enumerationTag = et, S.enumerationValues = evs }
      -> chompNewline [i|
        .tag = #{tagToTagEnumStr et},
        .length = #{length evs},|]
    S.Record { S.recordFields = rs }
      -> chompNewline [i|
        .field_count = #{length rs},
        .fields = record_fields_#{ln}_#{ident2str n},|]
    S.Combination { S.combinationFields = cf, S.combinationTag = ct }
      -> chompNewline [i|
        .tag = #{tagToTagEnumStr ct},
        .field_count = #{length cf},
        .fields = combination_fields_#{ln}_#{ident2str n},|]
    S.Union { S.unionFields = uf, S.unionTag = ut }
      -> chompNewline [i|
        .tag = #{tagToTagEnumStr ut},
        .field_count = #{length uf},
        .fields = union_fields_#{ln}_#{ident2str n},|]
  where
    ln = unpack (S.specName s)

tagToTagEnumStr :: C.Tag -> String
tagToTagEnumStr C.T1 = "caut_tag_1"
tagToTagEnumStr C.T2 = "caut_tag_2"
tagToTagEnumStr C.T4 = "caut_tag_4"
tagToTagEnumStr C.T8 = "caut_tag_8"
