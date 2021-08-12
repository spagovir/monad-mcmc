\documentclass[12pt]{article}
%include polycode.fmt
\begin{document}
Let's start by getting the module declaration and import statements out of the way. 
\begin{code}
module Sampleable (Sampleable (Sampleable), sampleIO) where
import Sugar
import System.Random
\end{code}

We first want to handle distributions that can be sampled directly through independence sampling---ie, distributions without conditioning on likelihoods. We represent sampling a value of type |a| as consuming a stream of random |Double| values in the interval $(0,1)$ and calculating a value of |a|:
\begin{code}

data Sampleable a = Sampleable {sample :: [Double] -> ([Double], a)}
\end{code}

Probability distributions have a clear monadic structure. We can identify a value $x$ with the random variable $X$ such that $X=x$ with certainty, and a distribution over distributions over a space $A$ clearly induces a distribution over $A$. 

Implementing the monadic structure is fairly straightforward: we can bind $\Pr(X)$ to $\Pr(Y||X)$ by first sampling $X=x$, and then pass on the remainder of our stream to the sampler for $\Pr(Y||X=x)$. The |Applicative| and |Functor| definitions then follow. 
\begin{code}

instance Monad Sampleable where
  return = Sampleable . flip (,)
  Sampleable g >>= f = Sampleable $ (\rs -> 
    let (rs', a) = g rs in 
      (sample $ f a) rs')

instance Applicative Sampleable where
  pure = return 
  rf <*> rv = Sampleable sample' where 
    sample' ds = 
      let
        (ds', f) = sample rf $ ds
      in 
        fmap f $ sample rv ds'

instance Functor Sampleable where 
  fmap f (Sampleable g) = Sampleable (\rs -> fmap f $ g rs)
\end{code} 

Finally we would like the ability to sample a Sampleable using the system IO monad:
\begin{code}
sampleIO :: Sampleable a -> IO a
sampleIO s = do
  gen <- newStdGen
  gen |> randomRs (0,1) |> sample s |> snd |> return   
\end{code}


\end{document}
