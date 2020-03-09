module Q2
    ( initialState
    , controlInput
    , repeatKFStep
    , toStateX
    , toGT
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
                           , stateGT :: State
                           , stateP :: Covariance } deriving (Show, Eq)
randProcess :: [Float]
randProcess = mkNormals 1234

randMeasure :: [Float]
randMeasure = mkNormals 4321

dt :: Float
dt = 0.10

time :: [Float]
time = iterate (+ dt) dt

controlInput :: [Control]
controlInput = map (\x -> V2 (10 * sin x) (0.01)) time

initialState :: StatePose
initialState = StatePose (V4 0 0.1 0.0349 0.5236) (V4 0 0.1 0.0349 0.5236) (identity !!* 0.01 :: Covariance)

rmat :: Covariance
rmat = V4 (V4 0.001 0 0 0)
          (V4 0 0.001 0 0)
          (V4 0 0 0.001 0)
          (V4 0 0 0 0.001)

qmat :: M22 Float
qmat = V2 (V2 0.1 0)
          (V2 0 0.3)

createMeasNoise :: Int -> Measurement
createMeasNoise i = (sqrt <$> qmat) !* randVec
  where randVec = V2 (randMeasure !! (i * 2)) (randMeasure !! (i * 2 + 1))

createProcessNoise :: Int -> State
createProcessNoise i = (sqrt <$> rmat) !* randVec
  where randVec = V4 (randProcess !! (i * 4)) (randProcess !! (i * 4 + 1))
                     (randProcess !! (i * 4 + 2)) (randProcess !! (i * 4 + 3))

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

predict :: Control -> State -> StatePose -> StatePose
predict u pNoise sp = StatePose (updateStateX u (stateX sp)) (g + pNoise) ((f !*! p !*! (transpose f)) !+! rmat)
  where g = updateStateX u (stateGT sp)
        p = stateP sp
        f = getStateTransition u (stateX sp)

update :: Measurement -> StatePose -> StatePose
update z sp = StatePose (x + k !* y) g (((identity :: M44 Float) !-! (k !*! h)) !*! p)
  where x = stateX sp           :: State
        g = stateGT sp          :: State
        p = stateP sp           :: Covariance
        y = z - (hFunc x)       :: Measurement
        h = hJacobian x         :: M24 Float
        b = (h !*! p !*! (transpose h)) !+! qmat :: M22 Float
        k = p !*! (transpose h) !*! (inv22 b) :: M42 Float

fakeMeasurements :: State -> Measurement -> Measurement
fakeMeasurements x noise = hFunc x + noise

kfStep :: Control -> Measurement -> State -> StatePose -> StatePose
kfStep u mNoise pNoise sp = update z sp'
  where sp' = predict u pNoise sp
        z = fakeMeasurements (stateGT sp') mNoise

toStateX :: StatePose -> (Float, Float)
toStateX sp = (stateX sp ^._x, stateX sp ^._y)

toGT :: StatePose -> (Float, Float)
toGT sp = (stateGT sp ^._x, stateGT sp ^._y)

printState :: StatePose -> IO ()
printState sp = putStrLn ("X: " ++ show x ++ " Y: " ++ show y)
  where x = (stateX sp) ^._x
        y = (stateX sp) ^._y

repeatKFStep :: Int -> StatePose -> [Control] -> [StatePose]
repeatKFStep n' sp' inp' = repeatKFStep' n' sp' inp' 0

repeatKFStep' :: Int -> StatePose -> [Control] -> Int -> [StatePose]
repeatKFStep' n sp inp i
  | i == n = []
  | otherwise = sp' : repeatKFStep' n sp' fs (i+1)
  where (f : fs) = inp
        mNoise = createMeasNoise i
        pNoise = createProcessNoise i
        sp' = kfStep f mNoise pNoise sp

