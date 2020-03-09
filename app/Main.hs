module Main where

import           Graphics.EasyPlot
import qualified Q2
import qualified Q3
import qualified Q4a
import qualified Q4b

q2 :: IO Bool
q2 = plot (PNG "image/q2.png") $ [ Data2D [Title "Estimate", Color Red, Style Lines] [] (map Q2.toStateX states)
                ] --, Data2D [Title "Ground Truth", Color Green, Style Lines] [] (map Q2.toGT states)]
  where states = Q2.repeatKFStep 100 Q2.initialState Q2.controlInput

q3 :: IO Bool
q3 = do
  _ <- plot (PNG "image/q3_traj.png")
    [ Data2D [Title "Original", Color Red, Style Lines] [] (map Q3.toTup cleanStates)
    , Data2D [Title "2%/2%", Color Blue, Style Lines] [] (map Q3.toTup noisyState1)
    , Data2D [Title "3%/2%", Color Green, Style Lines] [] (map Q3.toTup noisyState2)]
  _ <- plot (PNG "image/q3_rmse.png")
    [ Data2D [Title "2%/2%", Color Red, Style Points] [] (Q3.getRMSE cleanStates noisyState1)
    , Data2D [Title "3%/2%", Color Blue, Style Points] [] (Q3.getRMSE cleanStates noisyState2)]
  return True
  where cleanControl = Q3.getControl Q3.leftEncoder Q3.rightEncoder
        noisyControl1 = Q3.getControl (map (* 1.02) Q3.leftEncoder) (map (* 1.02) Q3.rightEncoder)
        noisyControl2 = Q3.getControl (map (* 1.03) Q3.leftEncoder) (map (* 1.02) Q3.rightEncoder)
        cleanStates = Q3.repeatStep 100 Q3.initialState cleanControl
        noisyState1 = Q3.repeatStep 100 Q3.initialState noisyControl1
        noisyState2 = Q3.repeatStep 100 Q3.initialState noisyControl2

q4 :: IO Bool
q4 = plot (PNG "image/q4.png") $ [ Data2D [Title "Estimate A", Color Red, Style Lines] [] (map Q4a.toStateX statesA)
                --, Data2D [Title "Ground Truth A", Color Orange, Style Lines] [] (map Q4a.toGT statesA)
                , Data2D [Title "Estimate B", Color Green, Style Lines] [] (map Q4b.toStateX statesB)
                ] --, Data2D [Title "Ground Truth B", Color Blue, Style Lines] [] (map Q4b.toGT statesB)]
  where statesA = Q4a.repeatKFStep 200 Q4a.initialState Q4a.controlInput
        statesB = Q4b.repeatKFStep 200 Q4b.initialState Q4b.controlInput

main :: IO ()
main = sequence_ $ [q3]

