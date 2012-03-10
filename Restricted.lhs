Restricted
==========

To be able to restrict monads, categories and arrows we use the rmonad package from Hackage and add restricted categories and restricted arrows as follows. We need the following Haskell language extensions

> {-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

The restricted version of the category and arrow type class (@RCategory@, @RArrow@) are in the module @Restricted@.

> module Restricted where
> import Prelude hiding ((.)) -- TODO Do we need this?

First we need do define a type family for constraints on two type parameters. This is analogous to the suitable library on Hackage. 

> data family Constraints2 (m :: * -> * -> *) :: * -> * -> *
> 
> class Suitable2 m a b where
>   constraints2 :: Constraints2 m a b
> 
> withResConstraints2 :: Suitable2 m a b                          
>                     => (Constraints2 m a b -> m a b) 
>                     -> m a b
> withResConstraints2 f = f constraints2
> 
> withConstraintsOf2 :: Suitable2 m a b 
>                    => m a b 
>                    -> (Constraints2 m a b -> k) 
>                    -> k
> withConstraintsOf2 _ f = f constraints2
> 

Categories
----------

The category definition itself is not different from @Data.Category@ except, that it allows for constraints in the form of @Suitable2@.

> infixr 9 .
> infixr 1 >>>, <<<
>
> class RCategory (~>) where
>   id :: (Suitable2 (~>) a a) => (~>) a a
>   (.) :: (Suitable2 (~>) b c, Suitable2 (~>) a b, Suitable2 (~>) a c) => (~>) b c -> (~>) a b -> (~>) a c

And the trivial aliases for composition:

> (<<<) :: (Suitable2 (~>) b c, 
>           Suitable2 (~>) a b, 
>           Suitable2 (~>) a c, 
>           RCategory (~>)) 
>           => (~>) b c 
>           -> (~>) a b 
>           -> (~>) a c
> (<<<) = (.)

And right composition:

> (>>>) :: (Suitable2 (~>) a b, 
>           Suitable2 (~>) b c, 
>           Suitable2 (~>) a c, 
>           RCategory (~>)) 
>           => (~>) a b 
>           -> (~>) b c 
>           -> (~>) a c
> f >>> g = g . f

Arrows
------

The arrow type class is defined in the same way as it is in @Control.Arrow@, however you can see that a lot of type constraints are in order to make sure every type is covered by the suitable type class. First the minimal required implementation.

> class RCategory a => RArrow a where
>  
>     arr :: (Suitable2 a b c) => (b -> c) -> a b c
>
>     first :: (Suitable2 a b c, Suitable2 a (b,d) (c,d)) => a b c -> a (b,d) (c,d)

And then the default implementations.

>     -- A mirror image of 'first'.
>     --
>     --   The default definition may be overridden with a more efficient
>     --   version if desired.
>     second :: (Suitable2 a b c, Suitable2 a (d,b) (d,c),
>                Suitable2 a (c, d) (d, c), Suitable2 a (b, d) (c, d),
>                Suitable2 a (b, d) (d, c),  Suitable2 a (d, b) (b, d)) => a b c -> a (d,b) (d,c)
>     second f = arr swap >>> first f >>> arr swap
>       where
>         swap :: (x,y) -> (y,x)
>         swap ~(x,y) = (y,x)


>     -- Split the input between the two argument arrows and combine
>     --   their output.  Note that this is in general not a functor.
>     --
>     --   The default definition may be overridden with a more efficient
>     --   version if desired.
>     (***) :: (Suitable2 a b c, Suitable2 a b' c', Suitable2 a (b,b') (c,c'),
>               Suitable2 a (c, b') (c, c'), Suitable2 a (b, b') (c, b'),
>               Suitable2 a (b', c) (c', c), Suitable2 a (c', c) (c, c'),
>               Suitable2 a (c, b') (b', c), Suitable2 a (b', c) (c, c')) => a b c -> a b' c' -> a (b,b') (c,c')
>     f *** g = first f >>> second g

  
>     -- Fanout: send the input to both argument arrows and combine
>     --   their output.
>     --
>     --   The default definition may be overridden with a more efficient
>     --   version if desired.
>     (&&&) :: (Suitable2 a b c, Suitable2 a b c', Suitable2 a b (c,c'),
>               Suitable2 a (b,b) (c,c'), Suitable2 a b (b,b),
>               Suitable2 a (b, b) (c, b), Suitable2 a (c, b) (c, c'),
>               Suitable2 a (c', c) (c, c'), Suitable2 a (b, c) (c', c),
>               Suitable2 a (b, c) (c, c'), Suitable2 a (c, b) (b, c))  => a b c -> a b c' -> a b (c,c')
>     f &&& g = arr (\b -> (b,b)) >>> f *** g

