module Lovefield.QueryExpr
  ( ExprTree(..)
  , ExprTreeUniversal(..)
  , QueryExpr(..)
  , QueryExprUniversal(..)
  , Query(), queryTable, runQuery
  ) where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Aff
import Data.Nullable
import Data.ArrayBuffer.Types
import Data.Leibniz
import Data.Date
import Data.Foreign
import Data.Function
import Data.Identity
import Lovefield.Internal.Exists
import Lovefield

data ExprTreeUniversal a t
  = From (Table t) (Leibniz a (t QueryExpr))

newtype ExprTree a
  = ExprTree (Exists2 (ExprTreeUniversal a))

fromExpr :: forall t . Table t -> ExprTree (t QueryExpr)
fromExpr table =
  ExprTree (mkExists2 (From table id))


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
  = QueryExpr (Exists0 (QueryExprUniversal a))


{-class HasLiterals a where
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
-}
data Query a
  = Pure (ExprTree a)


queryTable
  :: forall t
   . Table t
  -> Query (t QueryExpr)
queryTable table =
  Pure (fromExpr table)


foreign import data Selection :: *
foreign import data QueryBuilder :: *

foreign import selectAll :: Connection -> Selection
foreign import fromNative :: Fn3 Connection String Selection QueryBuilder
foreign import execNative
  :: forall eff a
   . Fn3
      QueryBuilder
      (Error -> Eff (db :: DB | eff) Unit)
      (Array a -> Eff (db :: DB | eff) Unit)
      (Eff (db :: DB | eff) Unit)

exec :: forall a eff . QueryBuilder -> Aff (db :: DB | eff) (Array a)
exec qb = makeAff (runFn3 execNative qb)

from :: Connection -> String -> Selection -> QueryBuilder
from = runFn3 fromNative

runQuery
  :: forall t eff
   . Connection
  -> Schema
  -> Query (t QueryExpr)
  -> Aff (db :: DB | eff) (Array (t Identity))
runQuery db schema (Pure (ExprTree et)) = runExists2 impl et
  where
    impl etu =
      case etu of
        From (Table name _ _) _ ->
          exec (from db name (selectAll db))
