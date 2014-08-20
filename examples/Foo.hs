module Foo where

data Foo a = Foo a Int a | Bar Float

newtype Baz = Baz (Foo String)

data Bl = Tr | Fl

foo2bool (Foo _ _ _) = Tr
foo2bool (Bar _) = Fl