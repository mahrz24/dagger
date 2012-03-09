% Dagger: Typed Graphical Models in Haskell
% Malte Harder



# Motivation

Graphical Models, directed acyclic graphs (DAGs) of random variables, are a powerful tool in Bayesian statistics, information theory, machine learning and probability theory in general. Current probability theory libraries in Haskell already provide an elegant syntax for simple probabilistic calculations. However, given more complex models the syntax not only becomes more clumsy: Calculations that depend on the graph structure of the model need a typed and labeled DAG data structure.

[TODO More motivation/introductions/examples]

We will show that labeled heterogeneously typed DAGs are Arrows and thus allow a very elegant syntax of specifying graphical models. Firstly though, we will reimplement the distribution Monad as an RMonad [as proposed in the Wiki] which allows us to use a map as backing data structure (with the monad restriction constraint Ord). This frees us from calling @norm@ and makes the implementation more efficient [why more efficient?]. Test Test

> {-# LANGUAGE RebindableSyntax, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
> import Data.Map
> import Data.Suitable
> import Control.RMonad
> import Prelude hiding (Monad, (>>=), fail, (.))
> import qualified Prelude as P

# Distribution RMonad

First we define a new type Dist as a map from an index (or event) type a and a probability type prob

> newtype Dist prob a = Dist { decons :: Map a prob }

and then implement the distribution constraints for the restricted monad

> data instance Constraints (Dist prob) a = Ord a => DistConstraints
> 
> instance Ord a => Suitable (Dist prob) a where
>   constraints = DistConstraints

Then we can implement the rmonad instance as follows

> certainly :: Num prob => a -> Dist prob a
> certainly x = Dist (singleton x 1)
> 
> instance Num prob => RMonad (Dist prob) where
>   return   = certainly
>   d >>= f  = withConstraintsOf d
>                   $ \ DistConstraints
>                       -> withResConstraints $ \ DistConstraints -> 
>                          Dist $ fromListWith (+) 
>                                 [(y,q*p) | (x,p) <- (toList P.. decons) d, 
>                                            (y,q) <- (toList P.. decons) (f x)]
>   fail _   = Dist empty

> instance (Ord prob, Ord a, Num prob, Show prob, Show a) => Show (Dist prob a) where
>    show = show P.. (\x -> (toAscList P.. decons) x)

The bind operator $a>>=(\backslash a -> b\, a)$ is equal to the following marginalization: [more explanations again]
$$ p(b) = \sum_a p(b \mid a) p(a). $$
Because we are using a restricted monad, we also have to use restricted categories and restricted arrows, which we first have to build. This is a restricted category implementation

We can use the restricted category to define a conditional distribution type 

> newtype Conditional prob a b = Conditional {  runConditional :: a -> Dist prob b }
> 
> type Source prob a = Conditional prob () a
> 
> 
> data instance Constraints2 (Conditional prob) a b = (Ord a, Ord b) => ConditionalConstraints
> 
> instance (Ord a, Ord b) => Suitable2 (Conditional prob) a b where
>   constraints2 = ConditionalConstraints
> 
> instance (Num prob) => RCategory (Conditional prob) where
>   id = Conditional (\a -> certainly a)
>   (.) cg@(Conditional g) cf@(Conditional f) = 
>     withConstraintsOf2 cg
>       (\ ConditionalConstraints -> withConstraintsOf2 cf
>         (\ ConditionalConstraints -> withResConstraints2
>           (\ ConditionalConstraints -> (Conditional (\a -> (f a) >>= g)))))

\appendix

\include{restricted}
