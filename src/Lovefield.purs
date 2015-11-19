module Lovefield where

import Prelude
import Data.Array (foldM)
import Data.Date
import Data.Either
import Data.Either.Unsafe (fromRight)
import Data.Exists
import Data.ArrayBuffer.Types
import Data.Foreign
import Data.Nullable
import Data.Foreign.Keys
import Data.Foreign.Index
import Data.Either
import Data.Identity
import Data.Leibniz
import Data.Function
import Data.Traversable
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Aff
import Lovefield.ColumnDescription

foreign import data DB :: !


data Table columns
  = Table String (Array Constraint) (columns ColumnDescription)


foreign import data ExistentialTable :: *

foreign import mkExistentialTable
  :: forall f
   . Table f
  -> ExistentialTable

foreign import runExistentialTable
  :: forall r
   . (forall f . Table f -> r)
  -> ExistentialTable
  -> r

data Schema
  = Schema String Int (Array ExistentialTable)


data LFType
  = LFInt
  | LFNumber
  | LFString
  | LFBoolean
  | LFDateTime
  | LFArrayBuffer
  | LFObject


data PrimaryKey
  = PrimaryKeys (Array String)
  | AutoIncrement String

data Constraint
  = PrimaryKey PrimaryKey
  | Unique String (Array String)
  | ForeignKey String


foreign import data Connection :: *
foreign import data SchemaBuilder :: *
foreign import data TableBuilder :: *

foreign import createNative
  :: forall eff
   . Fn2
      String
      Int
      (Eff (db :: DB | eff) SchemaBuilder)

foreign import createTableNative
  :: forall eff
   . Fn2
      String
      SchemaBuilder
      (Eff (db :: DB | eff) TableBuilder)

foreign import addColumnNative
  :: forall eff
   . Fn3
      String
      String -- This is lf.Type. Don't know of a cleaner way to do this
      TableBuilder
      (Eff (db :: DB | eff) TableBuilder)


foreign import addPrimaryKey
  :: forall eff
   . Fn3
      (Array String)
      Boolean
      TableBuilder
      (Eff (db :: DB | eff) TableBuilder)


foreign import addForeignKey
  :: forall eff
   . Fn2
      String
      TableBuilder
      (Eff (db :: DB | eff) TableBuilder)


foreign import addUnique
  :: forall eff
   . Fn3
      String
      (Array String)
      TableBuilder
      (Eff (db :: DB | eff) TableBuilder)


foreign import addNullable
  :: forall eff
   . Fn2
      (Array String)
      TableBuilder
      (Eff (db :: DB | eff) TableBuilder)


foreign import connectNative
  :: forall eff
   . Fn3
      SchemaBuilder
      (Error -> Eff (db :: DB | eff) Unit)
      (Connection -> Eff (db :: DB | eff) Unit)
      (Eff (db :: DB | eff) Unit)


foreign import insertOrReplaceNative
  :: forall eff values
   . Fn5
      Connection
      String
      (Array values)
      (Error -> Eff (db :: DB | eff) Unit)
      (Unit -> Eff (db :: DB | eff) Unit)
      (Eff (db :: DB | eff) Unit)


columnName :: forall a . ColumnDescription a -> String
columnName (ColumnDescription cd) = runExists impl cd
  where
    impl :: forall b . ColumnDescriptionUniversal a b -> String
    impl cdu =
      case cdu of
        Int name _ -> name
        Number name _ -> name
        String name _ -> name
        Boolean name _ -> name
        DateTime name _ -> name
        ArrayBuffer name _ -> name
        Object name _ -> name
        Nullable cd' _ -> columnName cd'

addColumn
  :: forall a eff
   . TableBuilder
  -> ColumnDescription a
  -> Eff (db :: DB | eff) TableBuilder
addColumn tb (ColumnDescription cd) = runExists impl cd
  where
    impl :: forall b . ColumnDescriptionUniversal a b -> Eff (db :: DB | eff) TableBuilder
    impl cdu =
      case cdu of
        Int name _ -> runFn3 addColumnNative name "int" tb
        Number name _ -> runFn3 addColumnNative name "number" tb
        String name _ -> runFn3 addColumnNative name "string" tb
        Boolean name _ -> runFn3 addColumnNative name "boolean" tb
        DateTime name _ -> runFn3 addColumnNative name "date" tb
        ArrayBuffer name _ -> runFn3 addColumnNative name "arraybuffer" tb
        Object name _ -> runFn3 addColumnNative name "object" tb
        Nullable cd' _ -> do
          tb' <- addColumn tb cd'
          runFn2 addNullable [columnName cd'] tb' -- Maybe we should batch all calls


addConstraint
  :: forall eff
   . TableBuilder
  -> Constraint
  -> Eff (db :: DB | eff) TableBuilder
addConstraint tb constraint =
  case constraint of
    PrimaryKey (PrimaryKeys keys) ->
      runFn3 addPrimaryKey keys false tb
    PrimaryKey (AutoIncrement key) ->
      runFn3 addPrimaryKey [key] true tb
    ForeignKey fk ->
      runFn2 addForeignKey fk tb
    Unique name columns ->
      runFn3 addUnique name columns tb


buildTable
  :: forall eff columnDescription
   . SchemaBuilder
  -> Table columnDescription
  -> Eff (db :: DB | eff) Unit
buildTable sb (Table name constraints columns) =
  let
    foreignColumns = toForeign columns
    ks = either (const []) id (keys foreignColumns)
    columnDescriptions :: forall a . Array (ColumnDescription a)
    columnDescriptions =
      map (unsafeFromForeign <<< fromRight <<< ((!) foreignColumns)) ks
    columnActions tb =
      foldM addColumn tb columnDescriptions
    constraintActions tb =
      foldM addConstraint tb constraints
  in
    do
      tb <- runFn2 createTableNative name sb
      tb' <- columnActions tb
      tb'' <- constraintActions tb'
      pure unit


connect
  :: forall eff
   . Schema
  -> Aff (db :: DB | eff) Connection
connect (Schema name version tables) = do
  sb <- liftEff $ runFn2 createNative name version
  liftEff $ traverse (runExistentialTable (buildTable sb)) tables
  makeAff (runFn3 connectNative sb)


insertOrReplace
  :: forall eff columns
   . Connection
  -> Schema
  -> Table columns
  -> Array (columns Identity)
  -> Aff (db :: DB | eff) Unit
insertOrReplace db (Schema schema _ _) (Table table _ _) values =
  makeAff (runFn5 insertOrReplaceNative db table values)


data Query a =
  From () (Leibniz a (t QueryExpr))


queryTable
  :: forall t
   . Table t
  -> Query (t QueryExpr)

runQuery
  :: forall t
   . Connection
  -> Schema
  -> Query (t QueryExpr)
  -> t Identity
