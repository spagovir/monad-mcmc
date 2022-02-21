\documentclass[12pt]{article}
\usepackage{pgfplots}
%include polycode.fmt
\begin{document}
Here we test several functions. 
\begin{code}
import Plot.Histogram
import Sampleable
import qualified Sampleable.Dist as SDist  
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Csv
import Inferable
import Inferable.MH
import qualified Inferable.Dist as IDist

main :: IO ()
main = do
\end{code}
Let's test the normal distribution.
\begin{code}
  putStrLn "Testing normals..."
  normals <- sampleIO $ replicateM 10000 $ SDist.normal 10 10   
  B.writeFile "./test/testFiles/normals.csv" $ encode $ histStep 0.5 normals
  putStrLn "Completed Normals Test"
\end{code}
And now let's plot the results from our test: 

\begin{tikzpicture}
\begin{axis} 
\addplot table[col sep = comma, header = false, x index = 0, y index = 1]{./testFiles/normals.csv}; 
\end{axis}
\end{tikzpicture}

Let's test a normal distribution conditioned on being $\geq 8$ with the same mean and variance:
\begin{code}
  putStrLn "Testing conditioned normals..."
  conditionedNormals <- sampleIO $ mh 100000 1 $ do
    x <- IDist.normal 10 10
    condition $ x >= 8 
    return x 
  B.writeFile "./test/testFiles/conditionedNormals.csv" $ encode $ histStep 0.5 $ take 90000 $ conditionedNormals 
  putStrLn "Completed conditioned normals test."
\end{code} 
And now we plot:

\begin{tikzpicture}
\begin{axis} 
\addplot table[col sep = comma, header = false, x index = 0, y index = 1]{./testFiles/conditionedNormals.csv}; 
\end{axis}
\end{tikzpicture}
\end{document}
