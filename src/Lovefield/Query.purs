module Lovefield.Query
  ( Query(), from
  , runQuery
  , Expr()
  ) where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Aff
import Control.Monad.State
import Data.Function
import Data.Identity
import Lovefield.Internal.Exists
import Lovefield.Internal.PrimExpr
import Lovefield.CanSwitchContext
import Lovefield
import Lovefield.App
import Lovefield.ColumnDescription
import Lovefield.Schema

newtype Expr a
  = Expr PrimExpr


type QueryState =
  { alias :: Alias
  , query :: PrimQuery
  }


initialState :: QueryState
initialState =
  { alias : 0
  , query : EmptyQuery
  }


newtype Query a
  = Query (State QueryState a)


newAlias :: State QueryState Int
newAlias = do
  state <- get
  put (state { alias = state.alias + 1 })
  pure state.alias


(>>-)
  :: forall record1 record2
   . (CanSwitchContext record1, CanSwitchContext record2) -- Don't know if we need these
  => Query (record1 Expr)
  -> (record1 Expr -> Query (record2 Expr))
  -> Query (record2 Expr)
(>>-) (Query state) continuation = Query $ do
  r1 <- state
  case continuation r1 of
    Query state' -> state'


from
  :: forall t
   . (CanSwitchContext t)
  => Table t
  -> Query (t Expr)
from tbl@(Table name _ cd) = Query $ do
  alias <- newAlias
  modify \qs ->
    qs { query = Times qs.query (BaseTable alias (mkExists2 tbl)) }
  pure (switchContext (columnDescriptionToExpr alias) cd)


columnDescriptionToExpr
  :: forall a . Alias -> App a ColumnDescription -> App a Expr
columnDescriptionToExpr alias (App cd) =
  App (Expr (AttrExpr alias (columnName cd)))


select
  :: forall t
   . t Expr
  -> Query (t Expr)
select expr = Query (pure expr)


{-where_ :: Expr Boolean -> Query Unit
where_ (Expr predicate) = Query $ do
  modify \qs ->
    qs { query = Restrict predicate qs.query }
  pure unit-}


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


runQuery
  :: forall t eff
   . (CanSwitchContext t)
  => Connection
  -> Query (t Expr)
  -> Aff (db :: DB | eff) (Array (t Identity))
runQuery db (Query state) = execute qs.query
  where
    qs =
      execState state initialState

    execute query =
      case query of
        Times _ query' ->
          execute query'
        BaseTable alias tbl ->
          runExists2 (\(Table name _ _) -> exec (runFn3 fromNative db name (selectAll db))) tbl
