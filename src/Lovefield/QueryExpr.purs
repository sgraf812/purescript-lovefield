module Lovefield.QueryExpr
  ( QueryExpr
  , HasLiterals()
  , IsNullable()
  ) where


import Data.Exists
import Data.Nullable
import Data.ArrayBuffer.Types
import Data.Leibniz
import Data.Date
import Data.Foreign
import Data.Maybe

data ExprTree a
  = Lit a
  | TableColumn String (ColumnDescription a)

-- TODO: Merge this with QueryExpr
data QueryExprUniversal a b
  = Int (ExprTree a) (Leibniz a Int)
  | Number (ExprTree a) (Leibniz a Number)
  | String (ExprTree a) (Leibniz a String)
  | Boolean (ExprTree a) (Leibniz a Boolean)
  | DateTime (ExprTree a) (Leibniz a Date)
  | ArrayBuffer (ExprTree a) (Leibniz a (Nullable ArrayBuffer))
  | Object (ExprTree a) (Leibniz a (Nullable Foreign))
  | Nullable (QueryExpr b) (Leibniz a (Nullable b))


newtype QueryExpr a
  = QueryExpr (Exists (QueryExprUniversal a))

mkQueryExpr -> forall a b . QueryExprUniversal a b -> QueryExpr a
mkQueryExpr quv = QueryExpr (mkExists quv)


class HasLiterals a where
  val :: a -> QueryExpr a

instance intHasLiterals :: HasLiterals Int where
  val n = QueryExpr (mkExists (Int (Lit n) id))

instance numberHasLiterals :: HasLiterals Number where
  val n = QueryExpr (mkExists (Number (Lit n) id))

instance stringHasLiterals :: HasLiterals String where
  val s = QueryExpr (mkExists (String (Lit s) id))

instance booleanHasLiterals :: HasLiterals Boolean where
  val b = QueryExpr (mkExists (Boolean (Lit b) id))

instance dateHasLiterals :: HasLiterals Date where
  val d = QueryExpr (mkExists (DateTime (Lit d) id))

-- TODO: Also write instances for the nullable cases

isNull :: QueryExpr (Nullable a) -> QueryExpr Bool
isNull expr =
  case expr of
    ArrayBuffer (Lit a) _ -> not (isJust (toMaybe a))
    Object (Lit a) _ -> not (isJust (toMaybe a))
    Nullable (Lit a) _ -> not (isJust (toMaybe a))
    -- _ -> no need for that case. GADTs would cover this


isNotNull :: QueryExpr (Nullable a) -> QueryExpr Bool
isNotNull expr =
  case expr of
    ArrayBuffer (Lit a) _ -> isJust (toMaybe a)
    Object (Lit a) _ -> isJust (toMaybe a)
    Nullable (Lit a) _ -> isJust (toMaybe a)


data Query a
  = Pure a


columnDescriptionToQueryExpr :: forall a . String -> ColumnDescription a -> QueryExpr a
columnDescriptionToQueryExpr tableName cd =
  let
    exprTree = TableColumn tableName cd
  in
    mkQueryExpr $
      case cd of
        CD.Int _ proof -> Int exprTree proof
        CD.Number _ proof -> Number exprTree proof
        CD.String _ proof -> String exprTree proof
        CD.Boolean _ proof -> Boolean exprTree proof
        CD.DateTime _ proof -> DateTime exprTree proof
        CD.ArrayBuffer _ proof -> ArrayBuffer exprTree proof
        CD.Object _ proof -> Object exprTree proof
        CD.Nullable cd' proof -> Nullable (TableColumn tableName cd') proof
        -- In this last case, we want to query just as if the column wasn't
        -- nullable. The information will be carried in the type system though
        -- and is relevant to e.g. `isNull`.


queryTable
  :: forall t
   . Table t
  -> Query (t QueryExpr)
queryTable (Table tableName constraints recordCD) = Pure recordQE
  where
    recordQE :: t QueryExpr
    recordQE =
      mapColumnFunctor (columnDescriptionToQueryExpr tableName) recordCD



runQuery
  :: forall t eff
   . Connection
  -> Schema
  -> Query (t QueryExpr)
  -> Aff (db :: DB | eff) (Array (t Identity))
runQuery db (Schema schemaName _ _) (Pure queryExpr) =
