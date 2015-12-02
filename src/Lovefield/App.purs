module Lovefield.App where


newtype App a f = App (f a)


runApp :: forall f a . App a f -> f a
runApp (App val) = val
