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

cDescriptorsFromSpec :: S.Specification -> String
cDescriptorsFromSpec s = unindent [i|
  #include "#{ln}_descriptors.h"

  struct type_descriptor type_descriptors[TYPE_COUNT_#{ln}] = {
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
      .prototype_tag = prototype_#{tps},
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

prototypeBody :: S.Specification -> S.Type -> String
prototypeBody s (S.Type { S.typeDesc = d }) =
  case d of
    S.Synonym { S.synonymRef = r }
      -> chompNewline [i|
        .ref_index = type_index_#{ln}_#{ident2str r},|]
    S.Range {}
      -> chompNewline [i|
        .offset = ???,
        .length = ???,
        .tag = ???,|]
    S.Array { S.arrayRef = r }
      -> chompNewline [i|
        .ref_index = type_index_#{ln}_#{ident2str r},
        .length = ???,|]
    S.Vector { S.vectorRef = r}
      -> chompNewline [i|
        .ref_index = type_index_#{ln}_#{ident2str r},
        .max_length = ???,|]
    S.Enumeration {}
      -> chompNewline [i|
        .tag = ???,
        .length = ???,|]
    S.Record {}
      -> chompNewline [i|
        .field_count = ???,
        .fields = ???,|]
    S.Combination {}
      -> chompNewline [i|
        .tag = ???,
        .field_count = ???,
        .fields = ???,|]
    S.Union {}
      -> chompNewline [i|
        .tag = ???,
        .field_count = ???,
        .fields = ???,|]
  where
    ln = unpack (S.specName s)
