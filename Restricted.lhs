Restricted
==========

> {-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

> module Restricted where
> import Prelude hiding ((.))

A restricted category implementation

> infixr 9 .
> infixr 1 >>>, <<<
> 
> data family Constraints2 (m :: * -> * -> *) :: * -> * -> *
> 
> class Suitable2 m a b where
>   constraints2 :: Constraints2 m a b
> 
> withResConstraints2 :: Suitable2 m a b => (Constraints2 m a b -> m a b) -> m a b
> withResConstraints2 f = f constraints2
> 
> withConstraintsOf2 :: Suitable2 m a b => m a b -> (Constraints2 m a b -> k) -> k
> withConstraintsOf2 _ f = f constraints2
> 
> class RCategory cat where
> 
>    id :: (Suitable2 cat a a) => cat a a
>    (.) :: (Suitable2 cat b c, Suitable2 cat a b, Suitable2 cat a c) => cat b c -> cat a b -> cat a c
> 
> (<<<) :: (Suitable2 cat b c, Suitable2 cat a b, Suitable2 cat a c, RCategory cat) => cat b c -> cat a b -> cat a c
> (<<<) = (.)
> 
> (>>>) :: (Suitable2 cat b c, Suitable2 cat a b, Suitable2 cat a c, RCategory cat) => cat a b -> cat b c -> cat a c
> f >>> g = g . f

What?
