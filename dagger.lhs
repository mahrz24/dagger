\documentclass[a4paper,10pt]{article}
%include polycode.fmt

\usepackage{graphicx,amssymb,amstext,amsmath}

\usepackage{tikz}
\usepackage{tkz-graph}

\usepackage{parskip}

\usepackage{mathpazo}
\usepackage{MinionPro}
\usepackage{MnSymbol}
\usepackage{inconsolata}
\renewcommand{\sfdefault}{Myriad-LF}

\title{Dagger: Typed Graphical Models in Haskell}

\author{Malte Harder}

\begin{document}

\maketitle

\long\def\ignore#1{}

\ignore{
\begin{code}
 {-# LANGUAGE RebindableSyntax, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
 import Data.Map
 import Data.Suitable
 import Control.RMonad
 import Prelude hiding (Monad, (>>=), fail, (.))
 import qualified Prelude as P
\end{code}
}
\section{Motivation}

Graphical Models, directed acyclic graphs (DAGs) of random variables, are a powerful tool in Bayesian statistics, information theory, machine learning and probability theory in general. Current probability theory libraries in Haskell already provide an elegant syntax for simple probabilistic calculations. However, given more complex models the syntax not only becomes more clumsy: Calculations that depend on the graph structure of the model need a typed and labeled DAG data structure.

[TODO More motivation/introductions/examples]

We will show that labeled heterogeneously typed DAGs are Arrows and thus allow a very elegant syntax of specifying graphical models. Firstly though, we will reimplement the distribution Monad as an RMonad [as proposed in the Wiki] which allows us to use a map as backing data structure (with the monad restriction constraint Ord). This frees us from calling @norm@ and makes the implementation more efficient [why more efficient?].

\section{Distribution RMonad}

First we define a new type Dist as a map from an index (or event) type a and a probability type prob
\begin{code}
 newtype Dist prob a = Dist { decons :: Map a prob }
\end{code}
and then implement the distribution constraints for the restricted monad
\begin{code}
 data instance Constraints (Dist prob) a = Ord a => DistConstraints
 
 instance Ord a => Suitable (Dist prob) a where
   constraints = DistConstraints
\end{code}
Then we can implement the rmonad instance as follows
\begin{code}
 certainly :: Num prob => a -> Dist prob a
 certainly x = Dist (singleton x 1)

 instance Num prob => RMonad (Dist prob) where
   return   = certainly
   d >>= f  = withConstraintsOf d
                   $ \ DistConstraints
                       -> withResConstraints $ \ DistConstraints -> 
                          Dist $ fromListWith (+) 
                                 [(y,q*p) | (x,p) <- (toList P.. decons) d, 
                                            (y,q) <- (toList P.. decons) (f x)]
   fail _   = Dist empty
\end{code}
\ignore{
\begin{code}
 instance (Ord prob, Ord a, Num prob, Show prob, Show a) => Show (Dist prob a) where
    show = show P.. (\x -> (toAscList P.. decons) x)
\end{code}
}
The bind operator $a>>=(\backslash a -> b\, a)$ is equal to the following marginalization: [more explanations again]
\[ p(b) = \sum_a p(b \mid a) p(a). \]
Because we are using a restricted monad, we also have to use restricted categories and restricted arrows, which we first have to build. This is a restricted category implementation
\begin{code}
 infixr 9 .
 infixr 1 >>>, <<<

 data family Constraints2 (m :: * -> * -> *) :: * -> * -> *

 class Suitable2 m a b where
   constraints2 :: Constraints2 m a b

 withResConstraints2 :: Suitable2 m a b => (Constraints2 m a b -> m a b) -> m a b
 withResConstraints2 f = f constraints2

 withConstraintsOf2 :: Suitable2 m a b => m a b -> (Constraints2 m a b -> k) -> k
 withConstraintsOf2 _ f = f constraints2

 class RCategory cat where

    id :: (Suitable2 cat a a) => cat a a
    (.) :: (Suitable2 cat b c, Suitable2 cat a b, Suitable2 cat a c) => cat b c -> cat a b -> cat a c

 (<<<) :: (Suitable2 cat b c, Suitable2 cat a b, Suitable2 cat a c, RCategory cat) => cat b c -> cat a b -> cat a c
 (<<<) = (.)

 (>>>) :: (Suitable2 cat b c, Suitable2 cat a b, Suitable2 cat a c, RCategory cat) => cat a b -> cat b c -> cat a c
 f >>> g = g . f

\end{code}
We can use the restricted category to define a conditional distribution type
\begin{code}
 
 newtype Conditional prob a b = Conditional {  runConditional :: a -> Dist prob b }

 type Source prob a = Conditional prob () a

 
 data instance Constraints2 (Conditional prob) a b = (Ord a, Ord b) => ConditionalConstraints

 instance (Ord a, Ord b) => Suitable2 (Conditional prob) a b where
   constraints2 = ConditionalConstraints

 instance (Num prob) => RCategory (Conditional prob) where
   id = Conditional (\a -> certainly a)
   (.) cg@(Conditional g) cf@(Conditional f) = 
     withConstraintsOf2 cg
       (\ ConditionalConstraints -> withConstraintsOf2 cf
         (\ ConditionalConstraints -> withResConstraints2
           (\ ConditionalConstraints -> (Conditional (\a -> (f a) >>= g)))))

\end{code}

\end{document}