module Q2
    ( initialState
    , controlInput
    , repeatKFStep
    , toStateX
    , printState
    ) where

import Control.Lens.Getter ((^.))
import Data.Random.Normal

import           Linear
import           Linear.Matrix

type State = V4 Float
type Control = V2 Float
type Covariance = M44 Float
type Measurement = V2 Float

data StatePose = StatePose { stateX :: State
                           , stateP :: Covariance } deriving (Show, Eq)
randMeasure :: [Float]
randMeasure = mkNormals 11111

dt :: Float
dt = 0.10

time :: [Float]
time = iterate (+ dt) dt

controlInput :: [Control]
controlInput = map (\x -> V2 (10 * sin x) (0.01)) time

initialState :: StatePose
initialState = StatePose (V4 0 0.1 0.0349 0.5236) (identity !!* 0.01 :: Covariance)

rmat :: Covariance
rmat = identity

qmat :: M22 Float
qmat = V2 (V2 0.1 0)
          (V2 0 0.3)

createMeasNoise :: Int -> Float -> Measurement
createMeasNoise i q = (sqrt <$> qt) !* randVec
  where randVec = V2 (randMeasure !! (i * 2)) (randMeasure !! (i * 2 + 1))
        qt = qmat !!* q

updateStateX :: Control -> State -> State
updateStateX u x = x + V4 (dt * u ^._x * cos (x ^._w))
                          (dt * u ^._x * sin (x ^._w))
                          (dt * u ^._y)
                          (dt * u ^._x * tan (x ^._z + dt * u ^._y))

getStateTransition ::  Control -> State -> Covariance
getStateTransition u x = V4 (V4 1 0 0 (-dt * u ^._x * sin (x ^._w)))
                            (V4 0 1 0 (dt * u ^._x * cos (x ^._w)))
                            (V4 0 0 1 0 :: V4 Float)
                            (V4 0 0 (dt * u ^._x * ((tan (x ^._z + dt * u ^._y) ^ 2) + 1)) 1)


hFunc :: State -> Measurement
hFunc x = V2 (sqrt $ (x ^._x) ^ 2 + (x ^._y) ^ 2) (atan (x ^._y / x ^._x))

hJacobian :: State -> M24 Float
hJacobian xVec = V2 (V4 (x / sqrtxy) (y / sqrtxy) 0 0)
                    (V4 (-y / sqr_xy) (x / sqr_xy) 0 0)
  where x = xVec ^._x
        y = xVec ^._y
        sqr_xy = x ^ 2 + y ^ 2
        sqrtxy = sqrt sqr_xy

predict :: Control -> StatePose -> Float -> StatePose
predict u sp r = StatePose (updateStateX u (stateX sp)) ((f !*! p !*! (transpose f)) !+! rt)
  where p = stateP sp
        rt = rmat !!* r
        f = getStateTransition u (stateX sp)

update :: Measurement -> StatePose -> Float -> StatePose
update z sp q = StatePose (x + k !* y) (((identity :: M44 Float) !-! (k !*! h)) !*! p)
  where x = stateX sp           :: State
        p = stateP sp           :: Covariance
        y = z - (hFunc x)       :: Measurement
        h = hJacobian x         :: M24 Float
        b = (h !*! p !*! (transpose h)) !+! (qmat !!* q) :: M22 Float
        k = p !*! (transpose h) !*! (inv22 b) :: M42 Float

fakeMeasurements :: State -> Measurement -> Measurement
fakeMeasurements x noise = hFunc x + noise

kfStep :: Control -> Measurement -> StatePose -> Float -> Float -> StatePose
kfStep u mNoise sp r q = update z sp' q
  where sp' = predict u sp r
        z = fakeMeasurements (stateX sp') mNoise

toStateX :: StatePose -> (Float, Float)
toStateX sp = (stateX sp ^._x, stateX sp ^._y)

printState :: StatePose -> IO ()
printState sp = putStrLn ("X: " ++ show x ++ " Y: " ++ show y)
  where x = (stateX sp) ^._x
        y = (stateX sp) ^._y

repeatKFStep :: Int -> StatePose -> [Control] -> Float -> Float -> [StatePose]
repeatKFStep n' sp' inp' r q = repeatKFStep' n' sp' inp' r q 0

repeatKFStep' :: Int -> StatePose -> [Control] -> Float -> Float -> Int -> [StatePose]
repeatKFStep' n sp inp r q i
  | i == n = []
  | otherwise = sp' : repeatKFStep' n sp' fs r q (i+1)
  where (f : fs) = inp
        mNoise = createMeasNoise i q
        sp' = kfStep f mNoise sp r q
