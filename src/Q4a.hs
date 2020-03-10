module Q4a
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

type State = V3 Float
type Control = V2 Float
type Covariance = M33 Float
type Measurement = V3 Float

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
initialState = StatePose (V3 0 0 0) (identity !!* 0.01 :: Covariance)

rmat :: Covariance
rmat = identity

qmat :: Covariance
qmat = V3 (V3 0.5 0 0)
          (V3 0 0.5 0)
          (V3 0 0 1 :: V3 Float)

createMeasNoise :: Int -> Measurement
createMeasNoise i = (sqrt <$> qmat) !* randVec
  where randVec = V3 (randMeasure !! (i * 3)) (randMeasure !! (i * 3 + 1))
                     (randMeasure !! (i * 3 + 2))

updateStateX :: Control -> State -> State
updateStateX u x = x + V3 (dt * u ^._x * cos (x ^._z))
                          (dt * u ^._x * sin (x ^._z))
                          (dt * u ^._y)

getStateTransition ::  Control -> State -> Covariance
getStateTransition u x = V3 (V3 1 0 (-dt * u ^._x * sin (x ^._z)))
                            (V3 0 1 (dt * u ^._x * cos (x ^._z)))
                            (V3 0 0 1 :: V3 Float)

hFunc :: State -> Measurement
hFunc = id

hJacobian :: State -> M33 Float
hJacobian _ = identity

predict :: Control -> StatePose -> Float -> StatePose
predict u sp r = StatePose (updateStateX u (stateX sp)) ((f !*! p !*! (transpose f)) !+! rt)
  where p = stateP sp
        rt = rmat !!* r
        f = getStateTransition u (stateX sp)

update :: Measurement -> StatePose -> StatePose
update z sp = StatePose (x + k !* y) (((identity :: M33 Float) !-! (k !*! h)) !*! p)
  where x = stateX sp           :: State
        p = stateP sp           :: Covariance
        y = z - (hFunc x)       :: Measurement
        h = hJacobian x         :: M33 Float
        b = (h !*! p !*! (transpose h)) !+! qmat :: M33 Float
        k = p !*! (transpose h) !*! (inv33 b) :: M33 Float

fakeMeasurements :: State -> Measurement -> Measurement
fakeMeasurements x noise = hFunc x + noise

kfStep :: Control -> Measurement -> StatePose -> Float -> StatePose
kfStep u mNoise sp r = update z sp'
  where sp' = predict u sp r
        z = fakeMeasurements (stateX sp') mNoise

toStateX :: StatePose -> (Float, Float)
toStateX sp = (stateX sp ^._x, stateX sp ^._y)

printState :: StatePose -> IO ()
printState sp = putStrLn ("X: " ++ show x ++ " Y: " ++ show y)
  where x = (stateX sp) ^._x
        y = (stateX sp) ^._y

repeatKFStep :: Int -> StatePose -> [Control] -> Float -> [StatePose]
repeatKFStep n' sp' inp' r = repeatKFStep' n' sp' inp' r 0 

repeatKFStep' :: Int -> StatePose -> [Control] -> Float -> Int -> [StatePose]
repeatKFStep' n sp inp r i
  | i == n = []
  | otherwise = sp' : repeatKFStep' n sp' fs r (i+1)
  where (f : fs) = inp
        mNoise = createMeasNoise i
        sp' = kfStep f mNoise sp r

