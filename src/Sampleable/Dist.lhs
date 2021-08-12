\documentclass[12pt]{article}
%include polycode.fmt
\begin{document}
Let's implement a few primitive distributions. These are sampled using their inverse cumulative distribution functions:
\begin{code}
module Sampleable.Dist (fromInverseCdf, uniform, categorical, bernoulli, normal) where
import Sampleable
import Data.Number.Erf

fromInverseCdf :: (Double -> a) -> Sampleable a 
fromInverseCdf f = Sampleable $ (\(r:rs) -> (rs, f r))
\end{code}
Note that we assume that our list of random doubles is infinite. 

Let's implememt |uniform|, |categorical|, and |normal| distributions: 
\begin{code}
uniform :: Double -> Double -> Sampleable Double
uniform a b = fromInverseCdf (\x -> (b-a) * x + a) 

categorical :: [(a, Double)] -> Sampleable a
categorical = fromInverseCdf . categoricalCdf where
  categoricalCdf ((b,t):bs) x = if x < t then b else categoricalCdf bs (x-t) 

bernoulli :: Double -> Sampleable Bool
bernoulli p = categorical [(True, p), (False, 1)]

normal :: Double -> Double -> Sampleable Double
normal m v =  fromInverseCdf $ fmap ((+m) . (*sqrt(v))) invnormcdf 
\end{code}
\end{docuent}
