module Main where

import           Graphics.EasyPlot
import qualified Q2
import qualified Q3
import qualified Q4a
import qualified Q4b

-- Problem 2 with Varying R
q2r :: IO Bool
q2r = plot (PNG "image/q2r.png") $ map (\(x, y) -> Data2D [Title ("R = " ++ show x), Color y, Style Lines] [] (map Q2.toStateX (states x 1.0))) [(0, Red), (0.001, Blue), (0.01, Green)]
  where states = Q2.repeatKFStep 100 Q2.initialState Q2.controlInput

-- Problem 2 with Varying Q
q2q :: IO Bool
q2q = plot (PNG "image/q2q.png") $
  map (\(x, y) -> Data2D [Title ("Q = " ++ show x ++ " * Qt"),
                          Color y,
                          Style Lines] [] (map Q2.toStateX (states 0 x)))
      [(0.5, Red), (1, Blue), (2, Green)]
  where states = Q2.repeatKFStep 100 Q2.initialState Q2.controlInput

-- Problem 3 trajectory and rmse plots
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

-- Problem 4 with measurement A varying R
q4ar :: IO Bool
q4ar = plot (PNG "image/q4ar.png") $ map (\(x, y) -> Data2D [Title ("R = " ++ show x), Color y, Style Lines] [] (map Q4a.toStateX (states x 1))) [(0, Red), (0.001, Blue), (0.01, Green)]
  where states = Q4a.repeatKFStep 200 Q4a.initialState Q4a.controlInput

-- Problem 4 with measurement B varying R
q4br :: IO Bool
q4br = plot (PNG "image/q4br.png") $ map (\(x, y) -> Data2D [Title ("R = " ++ show x), Color y, Style Lines] [] (map Q4b.toStateX (states x 1))) [(0, Red), (0.001, Blue), (0.01, Green)]
  where states = Q4b.repeatKFStep 200 Q4b.initialState Q4b.controlInput

-- Problem 4 with measurement A varying Q
q4aq :: IO Bool
q4aq = plot (PNG "image/q4aq.png") $ map (\(x, y) -> Data2D [Title ("Q = " ++ show x ++ " * Qt"), Color y, Style Lines] [] (map Q4a.toStateX (states 0 x))) [(0.5, Red), (1, Blue), (2, Green)]
  where states = Q4a.repeatKFStep 200 Q4a.initialState Q4a.controlInput

-- Problem 4 with measurement B varying Q
q4bq :: IO Bool
q4bq = plot (PNG "image/q4bq.png") $ map (\(x, y) -> Data2D [Title ("Q = " ++ show x ++ " * Qt"), Color y, Style Lines] [] (map Q4b.toStateX (states 0 x))) [(0.5, Red), (1, Blue), (2, Green)]
  where states = Q4b.repeatKFStep 200 Q4b.initialState Q4b.controlInput

main :: IO ()
main = sequence_ $ [q2r, q2q, q3, q4ar, q4br, q4aq, q4bq]

