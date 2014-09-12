{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , ExplicitForAll
           , MagicHash
           , UnboxedTuples
           , ExistentialQuantification
           , Rank2Types
  #-}
-- -fno-warn-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic data types and classes.
-- 
-----------------------------------------------------------------------------

-- #hide
module GHC.Base where

import GHC.Types
import GHC.Classes
import GHC.CString
import GHC.Prim

-- This is not strictly speaking required by this module, but is an
-- implicit dependency whenever () or tuples are mentioned, so adding it
-- as an import here helps to get the dependencies right in the new
-- build system.
import GHC.Tuple ()
-- Likewise we need Integer when deriving things like Eq instances, and
-- this is a convenient place to force it to be built
import GHC.Integer ()

infixr 9  .
infixr 5  ++
infixl 4  <$
infixl 1  >>, >>=
infixr 0  $

default ()              -- Double isn't available yet
{-
data  Bool  =  False | True
data Ordering = LT | EQ | GT 
data Char = C# Char#
type  String = [Char]
data Int = I# Int#
data  ()  =  ()
data [] a = MkNil

not True = False
(&&) True True = True
otherwise = True

build = error "urk"
foldr = error "urk"
-}

{- | The 'Functor' class is used for types that can be mapped over.
Instances of 'Functor' should satisfy the following laws:

> fmap id  ==  id
> fmap (f . g)  ==  fmap f . fmap g

The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
satisfy these laws.
-}

class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const

{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Minimal complete definition: '>>=' and 'return'.

Instances of 'Monad' should satisfy the following laws:

> return a >>= k  ==  k a
> m >>= return  ==  m
> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

Instances of both 'Monad' and 'Functor' should additionally satisfy the law:

> fmap f xs  ==  xs >>= return . f

The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.
-}

class  Monad m  where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
        -- Explicit for-alls so that we know what order to
        -- give type arguments when desugaring

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    -- | Fail with a message.  This operation is not part of the
    -- mathematical definition of a monad, but is invoked on pattern-match
    -- failure in a @do@ expression.
    fail        :: String -> m a

    {-# INLINE (>>) #-}
    m >> k      = m >>= \_ -> k
    fail s      = fail s

instance Functor ((->) r) where
    fmap = (.)

instance Monad ((->) r) where
    return = const
    f >>= k = \ r -> k (f r) r

instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

instance Functor [] where
    fmap = map

instance  Monad []  where
    m >>= k             = foldr ((++) . k) [] m
    m >> k              = foldr ((++) . (\ _ -> k)) [] m
    return x            = [x]
    fail _              = []

-- | 'foldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a list, reduces the list
-- using the binary operator, from right to left:
--
-- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     =  z
-- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE [0] foldr #-}
-- Inline only in the final stage, after the foldr/cons rule has had a chance
-- Also note that we inline it when it has *two* parameters, which are the 
-- ones we are keen about specialising!
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >    build g = g (:) []
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('build' g)@, which may arise after inlining, to @g k z@,
-- which avoids producing an intermediate list.

build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
        -- The INLINE is important, even though build is tiny,
        -- because it prevents [] getting inlined in the version that
        -- appears in the interface file.  If [] *is* inlined, it
        -- won't match with [] appearing in rules in an importing module.
        --
        -- The "1" says to inline in phase 1

build g = g (:) []

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >    augment g xs = g (:) xs
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('augment' g xs)@, which may arise after inlining, to
-- @g k ('foldr' k z xs)@, which avoids producing an intermediate list.

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE [1] augment #-}
augment g xs = g (:) xs

{-# RULES
"fold/build"    forall k z (g::forall b. (a->b->b) -> b -> b) . 
                foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) . 
                foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"                        foldr (:) [] = \x  -> x
"foldr/app"     [1] forall ys. foldr (:) ys = \xs -> xs ++ ys
        -- Only activate this from phase 1, because that's
        -- when we disable the rule that expands (++) into foldr

-- The foldr/cons rule looks nice, but it can give disastrously
-- bloated code when commpiling
--      array (a,b) [(1,2), (2,2), (3,2), ...very long list... ]
-- i.e. when there are very very long literal lists
-- So I've disabled it for now. We could have special cases
-- for short lists, I suppose.
-- "foldr/cons" forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)

"foldr/single"  forall k z x. foldr k z [x] = k x z
"foldr/nil"     forall k z.   foldr k z []  = z 

"augment/build" forall (g::forall b. (a->b->b) -> b -> b)
                       (h::forall b. (a->b->b) -> b -> b) .
                       augment g (build h) = build (\c n -> g c (h c n))
"augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .
                        augment g [] = build g
 #-}

-- This rule is true, but not (I think) useful:
--      augment g (augment h t) = augment (\cn -> g c (h c n)) t

-- | 'map' @f xs@ is the list obtained by applying @f@ to each element
-- of @xs@, i.e.,
--
-- > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- > map f [x1, x2, ...] == [f x1, f x2, ...]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f = \x ys -> c (f x) ys

-- The rules for map work like this.
-- 
-- Up to (but not including) phase 1, we use the "map" rule to
-- rewrite all saturated applications of map with its build/fold 
-- form, hoping for fusion to happen.
-- In phase 1 and 0, we switch off that rule, inline build, and
-- switch on the "mapList" rule, which rewrites the foldr/mapFB
-- thing back into plain map.  
--
-- It's important that these two rules aren't both active at once 
-- (along with build's unfolding) else we'd get an infinite loop 
-- in the rules.  Hence the activation control below.
--
-- The "mapFB" rule optimises compositions of map.
--
-- This same pattern is followed by many other functions: 
-- e.g. append, filter, iterate, repeat, etc.

{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g) 
  #-}

-- | Append two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- > [x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
--
-- If the first list is not finite, the result is the first list.

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++"    [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}


-- |'otherwise' is defined as the value 'True'.  It helps to make
-- guards more readable.  eg.
--
-- >  f x | x < 0     = ...
-- >      | otherwise = ...
otherwise               :: Bool
otherwise               =  True

-- | A 'String' is a list of characters.  String constants in Haskell are values
-- of type 'String'.
--
type String = [Char]

{-# RULES
"x# `eqChar#` x#" forall x#. x# `eqChar#` x# = True
"x# `neChar#` x#" forall x#. x# `neChar#` x# = False
"x# `gtChar#` x#" forall x#. x# `gtChar#` x# = False
"x# `geChar#` x#" forall x#. x# `geChar#` x# = True
"x# `leChar#` x#" forall x#. x# `leChar#` x# = True
"x# `ltChar#` x#" forall x#. x# `ltChar#` x# = False
  #-}

unsafeChr :: Int -> Char
unsafeChr (I# i#) = C# (chr# i#)

-- | The 'Prelude.fromEnum' method restricted to the type 'Data.Char.Char'.
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)

eqString :: String -> String -> Bool
eqString []       []       = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString _        _        = False

{-# RULES "eqString" (==) = eqString #-}
-- eqString also has a BuiltInRule in PrelRules.lhs:
--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

maxInt, minInt :: Int

{- Seems clumsy. Should perhaps put minInt and MaxInt directly into MachDeps.h -}
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#

-- | Identity function.
id                      :: a -> a
id x                    =  x

-- | The call '(lazy e)' means the same as 'e', but 'lazy' has a 
-- magical strictness property: it is lazy in its first argument, 
-- even though its semantics is strict.
lazy :: a -> a
lazy x = x
-- Implementation note: its strictness and unfolding are over-ridden
-- by the definition in MkId.lhs; in both cases to nothing at all.
-- That way, 'lazy' does not get inlined, and the strictness analyser
-- sees it as lazy.  Then the worker/wrapper phase inlines it.
-- Result: happiness

-- Assertion function.  This simply ignores its boolean argument.
-- The compiler may rewrite it to @('assertError' line)@.

-- | If the first argument evaluates to 'True', then the result is the
-- second argument.  Otherwise an 'AssertionFailed' exception is raised,
-- containing a 'String' with the source file and line number of the
-- call to 'assert'.
--
-- Assertions can normally be turned on or off with a compiler flag
-- (for GHC, assertions are normally on unless optimisation is turned on 
-- with @-O@ or the @-fignore-asserts@
-- option is given).  When assertions are turned off, the first
-- argument to 'assert' is ignored, and the second argument is
-- returned as the result.

--      SLPJ: in 5.04 etc 'assert' is in GHC.Prim,
--      but from Template Haskell onwards it's simply
--      defined here in Base.lhs
assert :: Bool -> a -> a
assert _pred r = r

breakpoint :: a -> a
breakpoint r = r

breakpointCond :: Bool -> a -> a
breakpointCond _ r = r

data Opaque = forall a. O a

-- | Constant function.
const                   :: a -> b -> a
const x _               =  x

-- | Function composition.
{-# INLINE (.) #-}
-- Make sure it has TWO args only on the left, so that it inlines
-- when applied to two functions, even if there is no final argument
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

-- | Application operator.  This operator is redundant, since ordinary
-- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
-- low, right-associative binding precedence, so it sometimes allows
-- parentheses to be omitted; for example:
--
-- >     f $ g $ h x  =  f (g (h x))
--
-- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
-- or @'Data.List.zipWith' ('$') fs xs@.
{-# INLINE ($) #-}
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x

-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
until                   :: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x       =  x
            | otherwise =  until p f (f x)

-- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
-- used as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf                :: a -> a -> a
asTypeOf                =  const

instance  Functor IO where
   fmap f x = x >>= (return . f)

instance  Monad IO  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k    = m >>= \ _ -> k
    return    = returnIO
    (>>=)     = bindIO

returnIO :: a -> IO a
returnIO x = IO $ \ s -> (# s, x #)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO $ \ s -> case m s of (# new_s, _ #) -> unIO k new_s

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

{-# INLINE getTag #-}
getTag :: a -> Int#
getTag x = x `seq` dataToTag# x

{-# INLINE quotInt #-}
{-# INLINE remInt #-}

quotInt, remInt, divInt, modInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)

quotRemInt :: Int -> Int -> (Int, Int)
(I# x) `quotRemInt` (I# y) = case x `quotRemInt#` y of
                             (# q, r #) ->
                                 (I# q, I# r)

divModInt :: Int -> Int -> (Int, Int)
(I# x) `divModInt` (I# y) = case x `divModInt#` y of
                            (# q, r #) -> (I# q, I# r)

divModInt# :: Int# -> Int# -> (# Int#, Int# #)
x# `divModInt#` y#
 | (x# ># 0#) && (y# <# 0#) = case (x# -# 1#) `quotRemInt#` y# of
                              (# q, r #) -> (# q -# 1#, r +# y# +# 1# #)
 | (x# <# 0#) && (y# ># 0#) = case (x# +# 1#) `quotRemInt#` y# of
                              (# q, r #) -> (# q -# 1#, r +# y# -# 1# #)
 | otherwise                = x# `quotRemInt#` y#

