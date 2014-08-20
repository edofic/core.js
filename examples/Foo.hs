module Foo where

data Foo a = Foo a Int a | Bar Float

newtype Baz = Baz (Foo String)

foo2bool (Foo _ _ _) = True
foo2bool (Bar _) = False