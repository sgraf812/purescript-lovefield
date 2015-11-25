module Lovefield.Internal.Exists where


foreign import data Exists0 :: (* -> *) -> *
foreign import data Exists1 :: ((* -> *) -> *) -> *
foreign import data Exists2 :: (((* -> *) -> *) -> *) -> *


foreign import mkExists0 :: forall f a . f a -> Exists0 f
foreign import mkExists1 :: forall f a . f a -> Exists1 f
foreign import mkExists2 :: forall f a . f a -> Exists2 f


foreign import runExists0 :: forall f r . (forall a . f a -> r) -> Exists0 f -> r
foreign import runExists1 :: forall f r . (forall a . f a -> r) -> Exists1 f -> r
foreign import runExists2 :: forall f r . (forall a . f a -> r) -> Exists2 f -> r
