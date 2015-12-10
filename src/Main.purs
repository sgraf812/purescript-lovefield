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
import Lovefield ((>>-), (.==.))
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

bert :: Names Identity
bert =
  Names
    { id : Identity 1
    , name : Identity "Bert"
    , age : Identity (toNullable Nothing)
    , bag : Identity (toNullable (Just (toForeign "blah")))
    }


mkNames id name age bag =
  Names { id : id, name : name, age : age, bag : bag }



instance namesCanSwitchContext :: CanSwitchContext Names where
  switchContext f (Names tf) =
    mkNames (f tf.id) (f tf.name) (f tf.age) (f tf.bag)


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
  LF.where_ (n1.name .==. n2.name) >>- \_ ->
  LF.select (Names n1)


main = launchAff do
  db <- LF.connect schema
  liftEff $ print "connected"
  LF.insertOrReplace db schema names [ bert ]
  liftEff $ print "inserted"
  result1 <- LF.runQuery db query1
  liftEff $ print (map name result1)
  result2 <- LF.runQuery db query2
  liftEff $ print (map name result2)
  --result <- LF.runQuery db schema query2
  --liftEff $ print result
