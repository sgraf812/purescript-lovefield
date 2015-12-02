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
import qualified Lovefield as LF
import qualified Lovefield.Schema as LF
import qualified Lovefield.ColumnDescription (column, nullable) as LF
import qualified Lovefield.Query as LF
import Lovefield.Internal.Exists
import Lovefield.CanSwitchContext
import Lovefield.App


newtype Names ctx =
  Names
    { id :: App Int ctx
    , name :: App String ctx
    , age :: App (Nullable Int) ctx
    , bag :: App (Nullable Foreign) ctx
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

value :: Names Identity
value =
  Names
    { id : App (Identity 1)
    , name : App (Identity "Bert")
    , age : App (Identity (toNullable Nothing))
    , bag : App (Identity (toNullable (Just (toForeign "blah"))))
    }


mkNames id name age bag =
  Names { id : id, name : name, age : age, bag : bag }



instance namesCanSwitchContext :: CanSwitchContext Names where
  --switchContext :: forall f g . (forall a . App a f -> App a g) -> Names f -> Names g
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
name (Names names) = runIdentity (runApp names.name)



query1 :: LF.Query (Names LF.Expr)
query1 = LF.from names


{-query2 :: Query (QueryExpr String)
query2 = do
  Names n <- LF.queryTable names
  pure n.name-}


main = launchAff do
  db <- LF.connect schema
  liftEff $ print "connected"
  LF.insertOrReplace db schema names [ value ]
  liftEff $ print "inserted"
  result <- LF.runQuery db query1
  liftEff $ print (map name result)
  --result <- LF.runQuery db schema query2
  --liftEff $ print result
