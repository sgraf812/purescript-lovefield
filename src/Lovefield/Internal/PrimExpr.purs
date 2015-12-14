module Lovefield.Internal.PrimExpr where

import Prelude
import Lovefield.Internal.Exists
import Lovefield.Schema
import Data.Tuple
import Data.List
import Data.Maybe

-- Taken (mostly) straight from HaskellDB

type Attribute = String
type Assoc = List (Tuple Attribute PrimExpr)
type Alias = Int
type Name = String


data SpecialOp
  = Order (Array OrderExpr)
	| Top Int
	| Offset Int


data OrderExpr
  = OrderExpr OrderOp PrimExpr


data OrderOp
  = OpAsc
  | OpDesc


data PrimExpr
  = AttrExpr  Alias Attribute
  | TernExpr  TernOp PrimExpr PrimExpr PrimExpr
  | BinExpr   BinOp PrimExpr PrimExpr
  | UnExpr    UnOp PrimExpr
  | AggrExpr  AggrOp PrimExpr
  | ConstExpr (Exists0 Literal)


type PrimExprMatcher r
  =  (Alias -> Attribute -> r)
  -> (TernOp -> PrimExpr -> PrimExpr -> PrimExpr -> r)
  -> (BinOp -> PrimExpr -> PrimExpr -> r)
  -> (UnOp -> PrimExpr -> r)
  -> (AggrOp -> PrimExpr -> r)
  -> (forall a . a -> r)
  -> PrimExpr
  -> r


matchOnPrimExpr :: forall r . PrimExprMatcher r
matchOnPrimExpr attr tern bin un aggr const expr =
  case expr of
    AttrExpr alias attribute -> attr alias attribute
    TernExpr op a b c -> tern op a b c
    BinExpr op a b -> bin op a b
    UnExpr op a -> un op a
    AggrExpr op expr -> aggr op expr
    ConstExpr ex -> runExists0 (\(Literal l) -> const l) ex
    --ListExpr exprs -> list exprs
    --ParamExpr par expr -> param par expr




newtype Literal a
  = Literal a


data TernOp
  = OpBetween


data BinOp
  = OpEq | OpLt | OpLtEq | OpGt | OpGtEq | OpNotEq
  | OpMatch
  | OpIn


data UnOp
	= OpIsNull
  | OpIsNotNull


data AggrOp
  = AggrCount | AggrSum
  | AggrAvg | AggrGeomMean
  | AggrMin | AggrMax
  | AggrStdDev
  | AggrDistinct
  | AggrGroup
