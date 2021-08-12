{-# LINE 6 "Main.lhs" #-}
module Main where
import Plot.Histogram
import Sampleable
import qualified Sampleable.Dist as SDist  
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Csv

main :: IO ()
main = do
{-# LINE 19 "Main.lhs" #-}
  normals <- sampleIO $ replicateM 100 $ SDist.normal 10 10   
  B.writeFile "normals.csv" $ encode $ histStep 1 normals
