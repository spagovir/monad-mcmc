\documentclass[12pt]{article}
\usepackage{pgfplots}
%include polycode.fmt
\begin{document}
Here we test several functions. 
\begin{code}
import Plot.Histogram
import Sampleable.Sampleable
import qualified Sampleable.Dist as SDist  
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Csv

main :: IO ()
main = do
\end{code}
Let's test the normal distribution.
\begin{code}
  normals <- sampleIO $ replicateM 10000 $ SDist.normal 10 10   
  B.writeFile "./test/testFiles/normals.csv" $ encode $ histStep 0.5 normals
\end{code}
And now let's plot the results from our test: 

\begin{tikzpicture}
\begin{axis} 
\addplot table[col sep = comma, header = false, x index = 0, y index = 1]{./testFiles/normals.csv}; 
\end{axis}
\end{tikzpicture}
\end{document}
