module Lovefield.Internal.PrimExpr where

import Prelude
import Lovefield.Internal.Exists

-- Taken (mostly) straight from HaskellDB

type Attribute = String
type Assoc = List (Attribute, PrimExpr)


data PrimQuery -- TODO: Maybe add Leibniz for encoding of types?
  = BaseTable TableName (List Attribute) -- From
  | Project Assoc PrimQuery -- Select
  | Restrict PrimExpr PrimQuery -- Where
  | Group Assoc PrimQuery


data SpecialOp
  = Order [OrderExpr]
	| Top Int
	| Offset Int


data OrderExpr
  = OrderExpr OrderOp PrimExpr


data OrderOp
  = OpAsc
  | OpDesc


data PrimExpr
  = AttrExpr  Attribute
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
