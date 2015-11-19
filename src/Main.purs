module Main where

import Prelude
import Data.Maybe
import Data.Either
import Data.Exists
import Data.Foreign
import Data.Nullable
import Data.Identity
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class
import Control.Monad.Aff
import qualified Lovefield as LF

newtype Names f =
  Names
    { id :: f Int
    , name :: f String
    , age :: f (Nullable Int)
    , bag :: f (Nullable Foreign)
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
    { id : Identity 1
    , name : Identity "Bert"
    , age : Identity (toNullable Nothing)
    , bag : Identity (toNullable (Just (toForeign "blah")))
    }

schema :: LF.Schema
schema =
  LF.Schema
    "MyDB"
    1
    [ LF.mkExistentialTable names
    ]

table :: forall t . LF.Table t -> Query (t LF.QueryExpr)

query = do
  Names names <- table T.names
  return names.age

main = launchAff do
  liftEff $ print "hi"
  db <- LF.connect schema
  liftEff $ print "success?"
  LF.insertOrReplace db schema names [ value ]
  liftEff $ print "success!"
