module Lovefield.Query
  ( Query(), from, (>>-), select, where_
  , runQuery
  , Expr(), (.==.), (./=.), (.<=.), (.<.), (.>=.), (.>.)
  , matches, in_
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
import Lovefield.App
import Lovefield.ColumnDescription
import Lovefield.Schema
import Lovefield.Native


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
  :: forall a record
   . (CanSwitchContext record) -- Don't know if we need these
  => Query a
  -> (a -> Query (record Expr))
  -> Query (record Expr)
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
  :: forall a . Alias -> ColumnDescription a -> Expr a
columnDescriptionToExpr alias cd =
  Expr (AttrExpr alias (columnName cd))


select
  :: forall t
   . t Expr
  -> Query (t Expr)
select expr = Query (pure expr)


where_ :: Expr Boolean -> Query Unit
where_ (Expr predicate) = Query $ do
  modify \qs ->
    qs { query = Restrict predicate qs.query }
  pure unit


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

-}

mkLiteral :: forall a . a -> Expr a
mkLiteral a = Expr (ConstExpr (mkExists0 (Literal a)))


binOp :: forall a b c. BinOp -> Expr a -> Expr b -> Expr c
binOp op (Expr a) (Expr b) = Expr (BinExpr op a b)


(.==.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.==.) a b = binOp OpEq a b


(./=.) :: forall a . Expr a -> Expr a -> Expr Boolean
(./=.) a b = binOp OpNotEq a b


(.<=.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.<=.) a b = binOp OpLtEq a b


(.<.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.<.) a b = binOp OpLt a b


(.>=.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.>=.) a b = binOp OpGtEq a b


(.>.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.>.) a b = binOp OpGt a b


matches :: String -> Expr String -> Expr Boolean
matches regex s = binOp OpMatch (mkLiteral regex) s


in_ :: forall a . Array a -> Expr a -> Expr Boolean
in_ values a = binOp OpIn (mkLiteral values) a


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


foreign import data LFExpr :: *

--primExprToLFExpr :: PrimExpr -> LFExpr
--primExprToLFExpr expr = case expr of
--  AttrExpr alias attribute ->



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
      exec (build (normalize query))

    build query =
      case query of
        BaseTable alias tbl ->
          runExists2 (\(Table name _ _) -> runFn3 fromNative db name (selectAll db)) tbl
        Restrict pred query ->
          build query -- TODO: Translate attribute access



normalize :: PrimQuery -> PrimQuery
normalize originalQuery =
  case originalQuery of
    -- Times reductions. Get them as far to the leafs
    -- (BaseTable, EmptyQuery) as possible.
    -- Cancel out EmptyQuery
    Times EmptyQuery q ->
      normalize q
    Times q EmptyQuery ->
      normalize q
    Times bt@(BaseTable _ _) q ->
      Times bt (normalize q)
    Times q bt@(BaseTable _ _) -> -- Commutation here is probably safe
      Times bt (normalize q)

    -- Pull Projections and Restrictions to the root
    Times (Project assoc q1) q2 ->
      normalize (Project assoc (Times q1 q2))
    Times q1 (Project assoc q2) ->
      normalize (Project assoc (Times q1 q2))

    Times (Restrict pred q1) q2 ->
      normalize (Restrict pred (Times q1 q2))
    Times q1 (Restrict pred q2) ->
      normalize (Restrict pred (Times q1 q2))

    -- Bring Products into a right associative normal form and normalize subqueries
    Times (Times q1 q2) q3->
      normalize (Times q1 (Times q2 q3))
    Times q1 q2 ->
      Times (normalize q1) (normalize q2)

    -- Restrictions. Get them to leafs, but above their products.
    Restrict pred (Project assoc q) ->
      normalize (Project assoc (Restrict pred q))
    Restrict pred q ->
      Restrict pred (normalize q)

    -- Projections. Merge consecutive ones.
    Project assoc1 (Project assoc2 q) ->
      normalize (Project (assoc1 ++ assoc2) q)
    Project assoc q ->
      Project assoc (normalize q)


    EmptyQuery ->
      EmptyQuery

    _ ->
      originalQuery
