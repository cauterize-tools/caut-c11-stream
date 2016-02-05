{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.HInfoDefines
       ( hInfoDefinesFromSpec
       ) where

import Cauterize.C11Stream.Util

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.List (intercalate)
import Data.Text (unpack)
import Numeric (showHex)

import qualified Cauterize.Specification as S
import qualified Cauterize.Hash as H

hInfoDefinesFromSpec :: S.Specification -> String
hInfoDefinesFromSpec s = unindent [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include <stdint.h>

  #include "#{ln}_descriptors.h"

  #define SCHEMA_FP_#{ln} {#{formatFp (S.specFingerprint s)}}

#{typeFps}

  #endif /* #{guardSym} */
|]
  where
    guardSym = [i|_CAUTERIZE_C11STREAM_#{ln}_INFO_DEFINES_|]
    ln = unpack (S.specName s)
    types = S.specTypes s
    typeFps = intercalate "\n" $ map fingerprint types
    fingerprint t =
      let n = ident2str ident
          ident = S.typeName t
          fp = formatFp (S.typeFingerprint t)
      in [i|  #define TYPE_FP_#{ln}_#{n} {#{fp}}|]

formatFp :: H.Hash -> String
formatFp f =
  let bs = H.hashToBytes f
      showByte n = case showHex n "" of
                     [a] -> ['0', 'x', '0', a]
                     [a,b] -> ['0', 'x', a, b]
                     _ -> error "formatFp: should be impossible"
  in intercalate "," (map showByte bs)
