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

import qualified Data.Map as M
import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C

cDescriptorsFromSpec :: S.Specification -> String
cDescriptorsFromSpec s = unindent [i|
  #include "#{ln}_descriptors.h"
  #include "#{ln}_types.h"

  #include "cauterize_iterators.h"

  #include <stdbool.h>
  #include <stdint.h>
  #include <stddef.h>

  #define ARR_ELEM_SPAN(TYPE) \\
    (uintptr_t)&(((TYPE *)NULL)[1])

#{fieldSets s types}

  struct type_descriptor const type_descriptors_#{ln}[TYPE_COUNT_#{ln}] = {
#{descriptorList}
  };

  struct schema_descriptor const schema_descriptor_#{ln} = {
    .type_count = TYPE_COUNT_#{ln},
    .types = type_descriptors_#{ln},
  };
|]
  where
    ln = unpack (S.specName s)
    types = S.specTypes s
    descriptorList = intercalate "\n" $ map descriptor types

    -- Names to how you delcare that name
    n2declMap = let s' = S.specTypes s
                    d = map t2decl s'
                    n = fmap S.typeName s'
                in primDeclMap `M.union` M.fromList (zip n d)
    luDecl n = fromMaybe (error $ "Invalid name: " ++ unpack (C.unIdentifier n) ++ ".")
                         (M.lookup n n2declMap)

    descriptor t =
      let n = ident2str ident
          ident = S.typeName t
          tps = typeToPrimString t
      in chompNewline [i|
    { /* #{n} */
      .type_id = type_id_#{ln}_#{n},
      .obj_size = sizeof(#{luDecl ident}),
      .prototype_tag = caut_proto_#{tps},
      .prototype.c_#{tps} = {
#{prototypeBody luDecl s t}
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
#{intercalate "\n" $ map (prototypeField s name) fs}
  };
|]
  where
    ln = unpack (S.specName s)

prototypeField :: S.Specification -> String -> S.Field -> String
prototypeField _ _ S.EmptyField { S.fieldName = n, S.fieldIndex = ix }
  = chompNewline [i|
    { .field_index = #{ix}, .data = false, .ref_id = 0, .offset = 0 }, /* #{ident2str n} */|]
prototypeField s typeName S.DataField { S.fieldName = n, S.fieldIndex = ix, S.fieldRef = r }
  = chompNewline [i|
    { .field_index = #{ix}, .data = true, .ref_id = type_id_#{ln}_#{r'}, .offset = offsetof(struct #{typeName}, #{n'}) }, /* #{n'} */|]
  where
    ln = unpack (S.specName s)
    n' = ident2str n
    r' = ident2str r


prototypeBody :: (C.Identifier -> String) -> S.Specification -> S.Type -> String
prototypeBody luDecl s (S.Type { S.typeName = n, S.typeDesc = d }) =
  case d of
    S.Synonym { S.synonymRef = r }
      -> chompNewline [i|
        .ref_id = type_id_#{ln}_#{ident2str r},|]
    S.Range { S.rangeOffset = ro, S.rangeLength = rl, S.rangeTag = rt, S.rangePrim = rp }
      -> chompNewline [i|
        .offset = #{ro},
        .length = #{rl}u,
        .tag = #{tagToTagEnumStr rt},
        .word_size = #{C.sizeMax . C.primToSize $ rp},|]
    S.Array { S.arrayRef = r, S.arrayLength = al }
      -> chompNewline [i|
        .ref_id = type_id_#{ln}_#{ident2str r},
        .length = #{al}u,
        .elem_span = ARR_ELEM_SPAN(#{luDecl r}),|]
    S.Vector { S.vectorRef = r, S.vectorLength = vl, S.vectorTag = vt }
      -> chompNewline [i|
        .ref_id = type_id_#{ln}_#{ident2str r},
        .max_length = #{vl}u,
        .tag = #{tagToTagEnumStr vt},
        .elem_span = ARR_ELEM_SPAN(#{luDecl r}),
        .elem_offset = offsetof(struct #{ident2str n}, elems),|]
    S.Enumeration { S.enumerationTag = et, S.enumerationValues = evs }
      -> chompNewline [i|
        .tag = #{tagToTagEnumStr et},
        .length = #{length evs}u,|]
    S.Record { S.recordFields = rs }
      -> chompNewline [i|
        .field_count = #{length rs}u,
        .fields = record_fields_#{ln}_#{ident2str n},|]
    S.Combination { S.combinationFields = cf, S.combinationTag = ct }
      -> chompNewline [i|
        .tag = #{tagToTagEnumStr ct},
        .field_count = #{length cf}u,
        .fields = combination_fields_#{ln}_#{ident2str n},|]
    S.Union { S.unionFields = uf, S.unionTag = ut }
      -> chompNewline [i|
        .tag = #{tagToTagEnumStr ut},
        .field_count = #{length uf}u,
        .fields = union_fields_#{ln}_#{ident2str n},|]
  where
    ln = unpack (S.specName s)

tagToTagEnumStr :: C.Tag -> String
tagToTagEnumStr C.T1 = "caut_tag_8"
tagToTagEnumStr C.T2 = "caut_tag_16"
tagToTagEnumStr C.T4 = "caut_tag_32"
tagToTagEnumStr C.T8 = "caut_tag_64"
