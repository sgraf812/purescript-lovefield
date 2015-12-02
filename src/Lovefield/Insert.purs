module Lovefield.Insert (insertOrReplace) where

import Prelude
import Data.Identity
import Data.Function
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Lovefield.Native
import Lovefield.Schema


insertOrReplace
  :: forall eff columns
   . Connection
  -> Schema
  -> Table columns
  -> Array (columns Identity)
  -> Aff (db :: DB | eff) Unit
insertOrReplace db (Schema schema _ _) (Table table _ _) values =
  makeAff (runFn5 insertOrReplaceNative db table values)


foreign import insertOrReplaceNative
  :: forall eff values
   . Fn5
      Connection
      String
      (Array values)
      (Error -> Eff (db :: DB | eff) Unit)
      (Unit -> Eff (db :: DB | eff) Unit)
      (Eff (db :: DB | eff) Unit)
