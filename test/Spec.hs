{-# LINE 7 "Spec.lhs" #-}
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
{-# LINE 22 "Spec.lhs" #-}
  putStrLn "Testing normals..."
  normals <- sampleIO $ replicateM 10000 $ SDist.normal 10 10   
  B.writeFile "./test/testFiles/normals.csv" $ encode $ histStep 0.5 normals
  putStrLn "Completed Normals Test"
{-# LINE 37 "Spec.lhs" #-}
  putStrLn "Testing conditioned normals..."
  conditionedNormals <- sampleIO $ mh 100000 1 $ do
    x <- IDist.normal 10 10
    condition $ x >= 8 
    return x 
  B.writeFile "./test/testFiles/conditionedNormals.csv" $ encode $ histStep 0.5 $ take 90000 $ conditionedNormals 
  putStrLn "Completed conditioned normals test."
