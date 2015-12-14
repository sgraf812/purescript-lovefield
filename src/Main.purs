module Main where

import Prelude
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Nullable
import Data.Identity
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class
import Control.Monad.Aff
import Lovefield ((>>-), (.==.), (.<.), (.>.), val, valNotNull)
import qualified Lovefield as LF
import Lovefield.Internal.Exists
import Lovefield.CanSwitchContext
import Lovefield.App


newtype Names ctx =
  Names
    { id :: ctx Int
    , name :: ctx String
    , age :: ctx (Nullable Int)
    , bag :: ctx (Nullable Foreign)
    }


idNames id name age bag =
  mkNames (Identity id) (Identity name) (Identity age) (Identity bag)


mkNames id name age bag =
  Names { id : id, name : name, age : age, bag : bag }


instance namesCanSwitchContext :: CanSwitchContext Names where
  switchContext f (Names tf) =
    mkNames (f tf.id) (f tf.name) (f tf.age) (f tf.bag)


data T3 a b c ctx =
  T3 (ctx a) (ctx b) (ctx c)


idT3 a b c =
  mkT3 (Identity a) (Identity b) (Identity c)


mkT3 = T3


instance t3CanSwitchContext :: CanSwitchContext (T3 a b c) where
  switchContext f (T3 a b c) =
    mkT3 (f a) (f b) (f c)


names :: LF.Table Names
names = LF.Table "Names" constraints columnDescription
  where
    constraints =
      [ LF.PrimaryKey (LF.AutoIncrement "id") ]
    columnDescription =
      Names
        { id : LF.column "id"
        , name : LF.column "name"
        , age : LF.nullable (LF.column "age")
        , bag : LF.column "bag"
        }


bert1 :: Names Identity
bert1 =
  idNames 1 "Bert" (toNullable (Just 42)) (toNullable (Just (toForeign "blah")))


alice :: Names Identity
alice =
  idNames 2 "Alice" (toNullable (Just 26)) (toNullable (Just (toForeign true)))


bert2 :: Names Identity
bert2 =
  idNames 3 "Bert" (toNullable (Just 55)) (toNullable Nothing)


schema :: LF.Schema
schema =
  LF.Schema
    "MyDB"
    1
    [ mkExists2 names
    ]

name :: Names Identity -> String
name (Names names) = runIdentity names.name


query1 :: LF.Query (Names LF.Expr)
query1 =
  LF.from names >>- \(Names n) ->
  LF.where_ (n.name .==. n.name) >>- \_ ->
  LF.select (Names n)


query2 :: LF.Query (Names LF.Expr)
query2 =
  LF.from names >>- \(Names n1) ->
  LF.from names >>- \(Names n2) ->
  LF.where_ (n1.age .<. n2.age) >>- \_ ->
  LF.select (Names n1)


query3 :: LF.Query (Names LF.Expr)
query3 =
  LF.from names >>- \(Names n) ->
  LF.where_ (n.age .>. valNotNull 30) >>- \_ ->
  LF.select (Names n)


query4 :: LF.Query (T3 String Number Int LF.Expr)
query4 = LF.aggregate aggregator query
  where
    aggregator :: Names LF.Aggregate -> T3 String Number Int LF.Expr
    aggregator (Names n) =
      mkT3 (LF.groupBy n.name) (LF.avg n.age) (LF.count n.bag)

    query :: LF.Query (Names LF.Expr)
    query =
      LF.from names >>- \(Names n) ->
      LF.where_ (n.age .>. LF.valNotNull 30) >>- \_ ->
      LF.select (Names n)


query5 :: LF.Query (T3 Int Number Int LF.Expr)
query5 = LF.aggregate aggregator2 (LF.aggregate aggregator1 query)
  where
    aggregator1 :: Names LF.Aggregate -> T3 String Number Int LF.Expr
    aggregator1 (Names n) =
      mkT3 (LF.groupBy n.name) (LF.avg n.age) (LF.count n.bag)

    aggregator2 :: T3 String Number Int LF.Aggregate -> T3 Int Number Int LF.Expr
    aggregator2 (T3 name age bag) =
      mkT3 (LF.count name) (LF.avg age) (LF.sum bag)

    query :: LF.Query (Names LF.Expr)
    query =
      LF.from names >>- \(Names n) ->
      LF.where_ (n.age .>. LF.valNotNull 30) >>- \_ ->
      LF.select (Names n)


main = launchAff do
  db <- LF.connect schema
  liftEff $ print "connected"
  LF.insertOrReplace db schema names [ alice, bert1, bert2 ]
  liftEff $ print "inserted"
  result1 <- LF.runQuery db query1
  liftEff $ print (map name result1)
  result2 <- LF.runQuery db query2
  liftEff $ print (map name result2)
  result3 <- LF.runQuery db query3
  liftEff $ print (map name result3)
  result4 <- LF.runQuery db query4
  liftEff $ print (map (\(T3 name avgAge bag) -> runIdentity name ++ " " ++ show (runIdentity avgAge) ++ " " ++ show (runIdentity bag)) result4)
  result5 <- LF.runQuery db query5
  liftEff $ print (map (\(T3 name avgAge bag) -> show (runIdentity name) ++ " " ++ show (runIdentity avgAge) ++ " " ++ show (runIdentity bag)) result4)
