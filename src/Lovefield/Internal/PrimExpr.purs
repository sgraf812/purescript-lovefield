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


data PrimQuery -- TODO: Maybe add Leibniz for encoding of types?
  = BaseTable Alias (Exists2 Table) -- From
  | Project Assoc PrimQuery -- Select
  | Restrict PrimExpr PrimQuery -- Where
  | Times PrimQuery PrimQuery -- From
  | Group Assoc PrimQuery
  | EmptyQuery


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
  | ListExpr  (Array PrimExpr)
  | ParamExpr (Maybe Name) PrimExpr


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
