module Lovefield.CanSwitchContext where


import Lovefield.App


class CanSwitchContext t where
  switchContext
    :: forall f g
     . (forall a . App a f -> App a g)
    -> t f
    -> t g
