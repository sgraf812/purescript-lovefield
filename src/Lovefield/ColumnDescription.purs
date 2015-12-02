module Lovefield.ColumnDescription
  ( ColumnDescription(..), ColumnDescriptionUniversal(..), columnName
  , HasColumnDescription, column
  , IsNullable, nullable
  ) where


import Prelude
import Data.Nullable
import Data.ArrayBuffer.Types
import Data.Leibniz
import Data.Date
import Data.Foreign
import Lovefield.Internal.Exists


data ColumnDescriptionUniversal a b
  = Int String (Leibniz a Int)
  | Number String (Leibniz a Number)
  | String String (Leibniz a String)
  | Boolean String (Leibniz a Boolean)
  | DateTime String (Leibniz a Date)
  | ArrayBuffer String (Leibniz a (Nullable ArrayBuffer))
  | Object String (Leibniz a (Nullable Foreign))
  | Nullable (ColumnDescription b) (Leibniz a (Nullable b))


newtype ColumnDescription a
  = ColumnDescription (Exists0 (ColumnDescriptionUniversal a))


columnName :: forall a . ColumnDescription a -> String
columnName (ColumnDescription cd) = runExists0 impl cd
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


class HasColumnDescription a where
  column :: String -> ColumnDescription a

instance intHasColumnDescription :: HasColumnDescription Int where
  column name = ColumnDescription (mkExists0 (Int name id))

instance numberHasColumnDescription :: HasColumnDescription Number where
  column name = ColumnDescription (mkExists0 (Number name id))

instance stringHasColumnDescription :: HasColumnDescription String where
  column name = ColumnDescription (mkExists0 (String name id))

instance booleanHasColumnDescription :: HasColumnDescription Boolean where
  column name = ColumnDescription (mkExists0 (Boolean name id))

instance dateHasColumnDescription :: HasColumnDescription Date where
  column name = ColumnDescription (mkExists0 (DateTime name id))

instance arrayBufferHasColumnDescription :: HasColumnDescription (Nullable ArrayBuffer) where
  column name = ColumnDescription (mkExists0 (ArrayBuffer name id))

instance objectHasColumnDescription :: HasColumnDescription (Nullable Foreign) where
  column name = ColumnDescription (mkExists0 (Object name id))


class IsNullable a where
  nullable :: ColumnDescription a -> ColumnDescription (Nullable a)

defaultNullable
  :: forall a . ColumnDescription a -> ColumnDescription (Nullable a)
defaultNullable cd =
  ColumnDescription (mkExists0 (Nullable cd id))

instance intIsNullable :: IsNullable Int where
  nullable = defaultNullable

instance numberIsNullable :: IsNullable Number where
  nullable = defaultNullable

instance stringIsNullable :: IsNullable String where
  nullable = defaultNullable

instance booleanIsNullable :: IsNullable Boolean where
  nullable = defaultNullable

instance dateIsNullable :: IsNullable Date where
  nullable = defaultNullable
