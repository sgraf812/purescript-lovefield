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
import Data.Tuple
import Debug.Trace
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
select expr =
  Query (pure expr)


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


foreign import runQueryNative
  :: forall recordOfExpr a eff
   . Fn7
      Connection
      recordOfExpr
      (Array From)
      (Array Where)
      (forall r . PrimExprMatcher r)
      (Error -> Eff (db :: DB | eff) Unit)
      (Array a -> Eff (db :: DB | eff) Unit)
      (Eff (db :: DB | eff) Unit)


type From =
  { alias :: Int
  , name :: String
  }


type Where =
  { condition :: PrimExpr
  }




runQuery
  :: forall t eff
   . (CanSwitchContext t)
  => Connection
  -> Query (t Expr)
  -> Aff (db :: DB | eff) (Array (t Identity))
runQuery db (Query state) = execute qs.query
  where
    qs =
      snd finalState

    selected =
      fst finalState

    finalState =
      runState state initialState

    execute query =
      makeAff $ runFn7
        runQueryNative db selected (froms query) (wheres query) matchOnPrimExpr

    tableName =
      runExists2 (\(Table name _ _) -> name)

    froms query =
      case query of
        EmptyQuery ->
          []
        BaseTable alias tbl ->
          [{ alias : alias, name : tableName tbl }]
        Restrict _ q ->
          froms q
        Times q1 q2 ->
          froms q1 ++ froms q2
        Project _ q ->
          froms q

    wheres query =
      case query of
        EmptyQuery ->
          []
        BaseTable _ _ ->
          []
        Restrict expr q ->
          [{ condition : expr }] ++ wheres q
        Times q1 q2 ->
          wheres q1 ++ wheres q2
        Project _ q ->
          wheres q
