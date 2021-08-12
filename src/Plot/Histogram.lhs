\documentclass[12pt]{article}
%include polycode.fmt
\begin{document}
We want some utilities to bin data values into histograms. 

\begin{code}
module Plot.Histogram (histStep) where
import Data.Fixed
import Data.Word
-- import qualified Data.Map.Monoidal as Map
import qualified MonoidalMap as Map
import Sugar
import Data.Tuple
import Data.Semigroup
\end{code}

We want our final output to be a list $[(x,y)]$ where $x$ denotes the location of a value bracket and $y$ is the number of data points in that bracket: 
\begin{code}
type HistPoints a = [(a,Word)]
\end{code}

And we want to implement a function that, given a step size and a list of data points, bins those data points into a histogram with brackets of that size:
\begin{code}
histStep :: Real a => a -> [a] -> HistPoints a
\end{code}

Internally, a histogram is really a map from |Real| bracket labels to the number of data points in that bracket. To avoid problems with checking floating point equality, we want to divide out the step size and use |Integer| labelled brackets instead, and we'll multiply the step size back in when unpacking:
\begin{code}
type HistMap = Map.Map Integer (Sum Word)
unpackHist :: Real a => a -> HistMap -> HistPoints a 
unpackHist s = Map.toAssocs |>> map (fmap getSum |>> swap |>> fmap fromIntegral |>> fmap (*s) |>> swap) 
\end{code}

It remains to accumulate our datapoints into our map:
\begin{code}
histStep s = map ((`div'` s) |>> (`Map.singleton` Sum 1)) |>> mconcat |>> unpackHist s 
\end{code}




\end{document}
