{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.CDescriptors
       ( cDescriptorsFromSpec
       ) where

import Cauterize.C11Stream.Util

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import Data.List (intercalate)

import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C

cDescriptorsFromSpec :: S.Specification -> String
cDescriptorsFromSpec s = unindent [i|
  #include "#{ln}_descriptors.h"

  struct type_descriptor const type_descriptors[TYPE_COUNT_#{ln}] = {
#{descriptorList}
  };

|]
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
      .index = type_index_#{ln}_#{n},
      .prototype_tag = caut_proto_#{tps},
      .prototype.#{tps} = {
#{prototypeBody s t}
      },
    },|]


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

--prototypeFields :: S.Specification

prototypeBody :: S.Specification -> S.Type -> String
prototypeBody s (S.Type { S.typeName = n, S.typeDesc = d }) =
  case d of
    S.Synonym { S.synonymRef = r }
      -> chompNewline [i|
        .ref_index = type_index_#{ln}_#{ident2str r},|]
    S.Range { S.rangeOffset = ro, S.rangeLength = rl, S.rangeTag = rt }
      -> chompNewline [i|
        .offset = #{ro},
        .length = #{rl},
        .tag = #{tagToTagEnumStr rt},|]
    S.Array { S.arrayRef = r, S.arrayLength = al }
      -> chompNewline [i|
        .ref_index = type_index_#{ln}_#{ident2str r},
        .length = #{al},|]
    S.Vector { S.vectorRef = r, S.vectorLength = vl }
      -> chompNewline [i|
        .ref_index = type_index_#{ln}_#{ident2str r},
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
