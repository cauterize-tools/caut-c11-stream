{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Stream.Util
  ( t2decl
  , primDeclMap
  , ident2str
  , ident2decl
  , prim2c
  , chompNewline
  , tag2c
  , typeToPrimString
  , validCTok
  , specCName
  ) where

import Data.Text (unpack)
import Data.String.Interpolate
import Data.Maybe
import qualified Data.Map as M

import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S
import qualified Cauterize.Specification.Types as T

ident2decl :: S.Specification -> C.Identifier -> String
ident2decl spec ident = getIdent
  where
    getIdent = fromMaybe getUsrTypeIdent (ident `M.lookup` primDeclMap)
    getUsrTypeIdent = t2decl $ fromMaybe err (ident `M.lookup` (T.specTypeMap spec))
    err = error (ident2str ident ++ " is not a type in the specification.")

t2decl :: S.Type -> String
t2decl (S.Type { S.typeName = tname, S.typeDesc = t}) =
  case t of
    S.Synonym {} -> n
    S.Range {} -> n
    S.Array {} -> sn
    S.Vector {} -> sn
    S.Enumeration {} -> en
    S.Record {} -> sn
    S.Combination {} -> sn
    S.Union {} -> sn
  where
    n = unpack (C.unIdentifier tname)
    sn = [i|struct #{n}|]
    en = [i|enum #{n}|]

primDeclMap :: M.Map C.Identifier String
primDeclMap = fmap prim2c C.primMap

ident2str :: C.Identifier -> String
ident2str = unpack . C.unIdentifier

prim2c :: C.Prim -> String
prim2c C.PU8   = "uint8_t"
prim2c C.PU16  = "uint16_t"
prim2c C.PU32  = "uint32_t"
prim2c C.PU64  = "uint64_t"
prim2c C.PS8   = "int8_t"
prim2c C.PS16  = "int16_t"
prim2c C.PS32  = "int32_t"
prim2c C.PS64  = "int64_t"
prim2c C.PF32  = "float"
prim2c C.PF64  = "double"
prim2c C.PBool = "bool"

chompNewline :: String -> String
chompNewline ('\n':rest) = rest
chompNewline str = str

tag2c :: C.Tag -> String
tag2c C.T1 = "caut_tag8_t";
tag2c C.T2 = "caut_tag16_t";
tag2c C.T4 = "caut_tag32_t";
tag2c C.T8 = "caut_tag64_t";

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

validCTok :: String -> String
validCTok = map dash_to_underscore
  where
  dash_to_underscore '-' = '_'
  dash_to_underscore a = a

specCName :: S.Specification -> String
specCName = validCTok . unpack . S.specName
