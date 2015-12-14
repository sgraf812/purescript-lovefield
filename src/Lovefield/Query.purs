module Lovefield.Query
  ( Query(), from, (>>-), select, where_
  , runQuery
  , Expr(), (.==.), (./=.), (.<=.), (.<.), (.>=.), (.>.)
  , matches, in_
  , HasLiterals, val, valNotNull
  ) where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Aff
import Control.Monad.State
import Data.Array hiding (groupBy)
import Data.Either
import Data.Either.Unsafe (fromRight)
import Data.Foreign
import Data.Foreign.Index (prop)
import Data.Foreign.Keys (keys)
import Data.Function
import Data.Identity
import Data.Tuple
import Data.Date
import Data.Nullable
import Data.Maybe
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


type TableReference =
  { alias :: Int
  , name :: String
  }


type AttributeReference =
  { alias :: Int
  , name :: String
  }


type QueryState =
  { nextAlias :: Int
  , references :: Array TableReference
  , restrictions :: Array (Expr Boolean)
  , groupings :: Array AttributeReference
  }


initialState :: QueryState
initialState =
  { nextAlias : 0
  , references : []
  , restrictions : []
  , groupings : []
  }


addReference :: String -> State QueryState Int
addReference tableName = state impl
  where
    impl state =
      let
        ref =
          { alias : state.nextAlias
          , name : tableName
          }
      in
        Tuple
          state.nextAlias
          (state
            { nextAlias = state.nextAlias + 1
            , references = ref : state.references
            })


addRestriction :: Expr Boolean -> State QueryState Unit
addRestriction expr = modify impl
  where
    impl state =
      state { restrictions = expr : state.restrictions }


newtype Query a
  = Query (State QueryState a)


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
  alias <- addReference name
  pure (switchContext (columnDescriptionToExpr alias) cd)


columnDescriptionToExpr
  :: forall a . Int -> ColumnDescription a -> Expr a
columnDescriptionToExpr alias cd =
  Expr (AttrExpr alias (columnName cd))


select
  :: forall t
   . t Expr
  -> Query (t Expr)
select expr =
  Query (pure expr)


where_ :: Expr Boolean -> Query Unit
where_ predicate =
  Query (addRestriction predicate)


newtype Aggregate a
  = Aggregate (Expr a)


unAggregate :: forall a . Aggregate a -> Expr a
unAggregate (Aggregate expr) = expr


aggregate
  :: forall record1 record2
   . (CanSwitchContext record1)
  => (record1 Aggregate -> record2 Expr)
  -> Query (record1 Expr)
  -> Query (record2 Expr)
aggregate aggregator (Query sourceQuery) = Query $ do
  record1 <- sourceQuery

  let
    record2 =
      aggregator (switchContext Aggregate record1)

    groupings =
      mapMaybe
        (\key ->
          case unsafeFromForeign (fromRight (prop key (toForeign record2))) of
            AttrExpr alias attr -> Just { alias : alias, name : attr }
            AggrExpr _ _ -> Nothing
            _ -> Nothing) -- this is actually an error
        (either (const []) id (keys (toForeign record2)))

  modify (_ { groupings = groupings } )
  pure record2


class HasLiterals a where
  val :: a -> Expr a
  valNotNull :: a -> Expr (Nullable a)

instance intHasLiterals :: HasLiterals Int where
  val = mkLiteral
  valNotNull = mkLiteral <<< toNullable <<< Just

instance numberHasLiterals :: HasLiterals Number where
  val = mkLiteral
  valNotNull = mkLiteral <<< toNullable <<< Just

instance stringHasLiterals :: HasLiterals String where
  val = mkLiteral
  valNotNull = mkLiteral <<< toNullable <<< Just

instance booleanHasLiterals :: HasLiterals Boolean where
  val = mkLiteral
  valNotNull = mkLiteral <<< toNullable <<< Just

instance dateHasLiterals :: HasLiterals Date where
  val = mkLiteral
  valNotNull = mkLiteral <<< toNullable <<< Just

instance listHasLiterals :: (HasLiterals a) => HasLiterals (Array a) where
  val = mkLiteral
  valNotNull = mkLiteral <<< toNullable <<< Just


mkLiteral :: forall a . a -> Expr a
mkLiteral a = Expr (ConstExpr (mkExists0 (Literal a)))


binOp :: forall a b c. BinOp -> Expr a -> Expr b -> Expr c
binOp op (Expr a) (Expr b) = Expr (BinExpr op a b)


(.==.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.==.) = binOp OpEq


(./=.) :: forall a . Expr a -> Expr a -> Expr Boolean
(./=.) = binOp OpNotEq


(.<=.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.<=.) = binOp OpLtEq


(.<.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.<.) = binOp OpLt


(.>=.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.>=.) = binOp OpGtEq


(.>.) :: forall a . Expr a -> Expr a -> Expr Boolean
(.>.) = binOp OpGt


matches :: String -> Expr String -> Expr Boolean
matches regex = binOp OpMatch (mkLiteral regex)


in_ :: forall a . Array a -> Expr a -> Expr Boolean
in_ values = binOp OpIn (mkLiteral values)


aggrExpr :: forall a . AggrOp -> Aggregate a -> Expr a
aggrExpr op (Aggregate (Expr primExpr)) =
  Expr (AggrExpr op primExpr)


groupBy :: forall a . Aggregate a -> Expr a
groupBy = unAggregate -- groupings are handled in QueryState


sum :: forall a . (Num a) => Aggregate a -> Expr a
sum = aggrExpr AggrSum


avg :: forall a . (Num a) => Aggregate a -> Expr a
avg = aggrExpr AggrAvg


min :: forall a . (Ord a) => Aggregate a -> Expr a
min = aggrExpr AggrMin


max :: forall a . (Ord a) => Aggregate a -> Expr a
max = aggrExpr AggrMax


stdDev :: forall a . (Num a) => Aggregate a -> Expr a
stdDev = aggrExpr AggrStdDev


count :: forall a . (Aggregate a) -> Expr a
count = aggrExpr AggrCount


distinct :: forall a . (Aggregate a) -> Expr a
distinct = aggrExpr AggrDistinct


foreign import runQueryNative
  :: forall recordOfExpr a eff
   . Fn8
      Connection
      recordOfExpr
      (Array TableReference)
      (Array (Expr Boolean))
      (Array AttributeReference)
      (forall r . PrimExprMatcher r)
      (Error -> Eff (db :: DB | eff) Unit)
      (Array a -> Eff (db :: DB | eff) Unit)
      (Eff (db :: DB | eff) Unit)


runQuery
  :: forall t eff
   . (CanSwitchContext t)
  => Connection
  -> Query (t Expr)
  -> Aff (db :: DB | eff) (Array (t Identity))
runQuery db (Query state) = result
  where
    finalState =
      runState state initialState

    qs =
      snd finalState

    selected =
      fst finalState

    result =
      makeAff $ runFn8
        runQueryNative
          db
          selected
          qs.references
          qs.restrictions
          qs.groupings
          matchOnPrimExpr
