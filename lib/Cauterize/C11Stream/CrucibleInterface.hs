{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.CrucibleInterface
       ( crucibleFromSpec
       ) where

import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)

crucibleFromSpec :: S.Specification -> String
crucibleFromSpec s = unindent [i|
  #include "#{ln}.h"
  #include "crucible_interface.h"

  size_t const schema_length_word_size = #{slws};
  size_t const schema_tag_size = #{sts};
  size_t const schema_max_size = #{sms};
|]
    where
    slws = C.sizeMax . C.tagToSize . S.specLengthTag $ s
    sts = S.specTypeLength s
    sms = C.sizeMax . S.specSize $ s
    ln = unpack (S.specName s)
