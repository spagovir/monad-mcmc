\documentclass[12pt]{article}
%include polycode.fmt
\begin{document}

We want to be able to implement distributions that can easily be conditionalized, and sample from them. To do this, we want to be able to define distributions $\Pr(A|B)$ as a function from $B$ to a distribution defined by a prior distribution over $P(A)$ and a log likelihood function representing $P(B|A)$. 

Alternatively, in situations where we have a distribution conditional on being confined in some lower dimensional subspace we would like to be able to reparametrize our sampling space and still be able to use an unnormalized likelihood function over our reparameterized coordinates in order to sample.

Both of these cased tend to require Markov Chain Monte Carlo methods for efficient sampling. These will tend to require us to keep traces of where we are in our product space. Therefore we will have to keep track of not only our output variable but also traces of every independent random variable and our overall total likelihood. 

In order to allow ourselves to use different Markov Chain Monte Carlo implementations, we will first implement prior distributions and log likelihood conditioning as forgetful functions and use a Free monad to join them together until we convert them into our Markov Chain Monte Carlo implementation monad of choice. Our log likelihood functions will be extended real valued in order to allow for probability zero values:

\begin{code}
data Extended r = NInfty | Finite r | PInfty deriving Show
instance (Num r, Ord r) => Num (Extended r) where 
  Finite r + Finite r' = Finite $ r + r'
  NInfty + Finite _ = NInfty
  Finite _ + NInfty = NInfty
  NInfty + NInfty = NInfty
  PInfty + Finite _ = PInfty  
  Finite _ + PInfty = PInfty 
  PInfty + PInfty = PInfty 
  Finite r * Finite r' = Finite $ r + r'
  Finite 0 * _ = Finite 0
  _ * Finite 0 = Finite 0
  Finite r * PInfty 
    | r > 0 = PInfty
    | r < 0 = NInfty
  PInfty * Finite r 
    | r > 0 = PInfty
    | r < 0 = NInfty
  Finite r * NInfty 
    | r > 0 = NInfty
    | r < 0 = PInfty 
  NInfty * Finite r 
    | r > 0 = NInfty
    | r < 0 = PInfty 
  negate (Finite r) = Finite $ negate r
  negate NInfty = PInfty
  negate PInfty = NInfty
  fromInteger = Finite . fromInteger
  abs (Finite r) = Finite $ abs r
  abs _ = PInfty 
  signum (Finite r) = Finite $ signum r
  signum NInfty = Finite $ negate 1
  signum PInfty = 1 

data InferF a b 
  = Prior {priorLikelihood :: a -> Extended Double, getPrior :: Sampleable (a,b)}
  | Likelihood (Extended Double) b
  deriving Functor

type MonadInfer = Free InferF 

fromSampleable :: Sampleable a -> MonadInfer a
fromSampleable = liftF . fmap (\x -> (x,x))

likelihood :: Extended Double -> MonadInfer () 
likelihood = flip Likelihood () |>> liftF 
\end{code}
