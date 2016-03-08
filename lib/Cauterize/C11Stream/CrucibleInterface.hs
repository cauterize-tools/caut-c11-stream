{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.CrucibleInterface
       ( crucibleFromSpec
       ) where

import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C
import Cauterize.C11Stream.Util (specCName)

import Data.String.Interpolate
import Data.String.Interpolate.Util

crucibleFromSpec :: S.Specification -> String
crucibleFromSpec s = unindent [i|
  #include "#{ln}.h"
  #include "#{ln}_info.h"
  #include "crucible_interface.h"

  size_t const schema_length_word_size = #{slws};
  size_t const schema_tag_size = #{sts};
  size_t const schema_max_size = #{sms};
  size_t const schema_depth = #{sd};
  struct schema_descriptor const * const schema_schema_descriptor = &schema_descriptor_#{ln};
  struct schema_info const * const schema_schema_info = &schema_info_#{ln};
|]
    where
    slws = C.sizeMax . C.tagToSize . S.specLengthTag $ s
    sts = S.specTypeLength s
    sms = C.sizeMax . S.specSize $ s
    sd = S.specDepth s
    ln = specCName s
