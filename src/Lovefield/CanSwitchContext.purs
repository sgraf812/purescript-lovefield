module Lovefield.CanSwitchContext where


class CanSwitchContext t where
  switchContext
    :: forall f g
     . (forall a . f a -> g a)
    -> t f
    -> t g
