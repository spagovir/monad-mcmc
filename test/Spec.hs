{-# LINE 7 "Spec.lhs" #-}
import Plot.Histogram
import Sampleable.Sampleable
import qualified Sampleable.Dist as SDist  
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Csv

main :: IO ()
main = do
{-# LINE 19 "Spec.lhs" #-}
  normals <- sampleIO $ replicateM 10000 $ SDist.normal 10 10   
  B.writeFile "./test/testFiles/normals.csv" $ encode $ histStep 0.5 normals
