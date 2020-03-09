module Q3
    ( leftEncoder
    , rightEncoder
    , getControl
    , initialState
    , toTup
    , repeatStep
    , getRMSE
    ) where

import Control.Lens.Getter ((^.))

import           Linear

type State = V3 Float
type Control = V2 Float

dt :: Float
dt = 0.10

time :: [Float]
time = iterate (+ dt) 0

leftEncoder :: [Float]
leftEncoder = map cos time

rightEncoder :: [Float]
rightEncoder = map sin time

dCenter :: Float -> Float -> Float
dCenter l r = (l + r) / 2

dTheta :: Float -> Float -> Float
dTheta l r = (r - l) / 0.45

diffSteps :: [Float] -> [Float]
diffSteps = zipWith (-) =<< tail

getControl :: [Float] -> [Float] -> [Control]
getControl ls rs = zipWith (\a b -> V2 (dCenter a b) (dTheta a b)) (diffSteps ls) (diffSteps rs)

initialState :: State
initialState = V3 0.1 0.2 0.7854

updateStateX :: State -> Control -> State
updateStateX x u = x + V3 (u ^._x * cos (x ^._z + u ^._y / 2))
                          (u ^._x * sin (x ^._z + u ^._y / 2))
                          (u ^._y)

toTup :: State -> (Float, Float)
toTup sp = (sp ^._x, sp ^._y)

repeatStep :: Int -> State -> [Control] -> [State]
repeatStep n' sp' inp' = repeatStep' n' sp' inp' 0

repeatStep' :: Int -> State -> [Control] -> Int -> [State]
repeatStep' n sp inp i
  | i == n = []
  | otherwise = sp' : repeatStep' n sp' us (i+1)
  where (u : us) = inp
        sp' = updateStateX sp u

getDistance :: State -> State -> Float
getDistance x1 x2 = euclidean (x1 ^._x - x2 ^._x) (x1 ^._y - x2 ^._y)
  where euclidean a b = sqrt (a * a + b * b)

getRMSE :: [State] -> [State] -> [(Float, Float)]
getRMSE sp1 sp2 = zip time (zipWith getDistance sp1 sp2)
