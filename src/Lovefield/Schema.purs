module Lovefield.Schema where


import Prelude
import Lovefield.Internal.Exists
import Lovefield.ColumnDescription


data Table columns
  = Table String (Array Constraint) (columns ColumnDescription)


data Schema
  = Schema String Int (Array (Exists2 Table))


data PrimaryKey
  = PrimaryKeys (Array String)
  | AutoIncrement String

data Constraint
  = PrimaryKey PrimaryKey
  | Unique String (Array String)
  | ForeignKey String
