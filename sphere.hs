module Sphere where

import Vector
import qualified Intersectable as I

data Sphere = Sphere{
    origin :: Vec,
    radius :: Double,
    color :: Vec
}

quadratic :: Double -> Double -> Double -> Maybe (Double, Double)
quadratic a b c 
    | disc < 0  = Nothing
    | otherwise = Just ((-b-sqrtDisc)/(2*a), (-b+sqrtDisc)/(2*a))
    where disc = b*b-4*a*c
          sqrtDisc = sqrt disc

-- (So-(Ro+t*Rd))^2 = r^2
-- So^2 - 2*So*(Ro+t*Rd) + (Ro+t*Rd)^2 = r^2
-- So^2 - 2*So*Ro - 2*So*t*Rd + Ro^2 + 2*t*Rd*Ro + t^2*Rd^2 - r^2 = 0
-- t^2*(Rd^2) + t*(2*Rd*Ro - 2*So*Rd) + (So^2 - 2*So*Ro + Ro^2 - r^2) = 0
-- a = Rd^2
-- b = 2*Rd*Ro - 2*So*Rd
-- c = So^2 - 2*So*Ro + Ro^2 - r^2
--   = (So-Ro)^2 - r^2

sphereIntersect :: Vec -> Vec -> Sphere -> Maybe I.Intersection
sphereIntersect ro rd Sphere{origin=origin, radius=radius, Sphere.color=color} = maybeDistance >>= \distance -> Just I.Intersection{I.color=color, I.pos=hit distance, I.normal=calcNormal origin (hit distance)}
    where
        hit distance = vecAdd ro $ vecMultiply rd distance
        calcNormal :: Vec -> Vec -> Vec
        calcNormal origin hit = vecNormalise $ vecSubtract hit origin
        qa = vecLengthSquared rd
        qb = 2*(vecDot rd ro - vecDot origin rd)
        qc = vecLengthSquared (vecSubtract origin ro) - radius*radius
        maybeDistance = quadratic qa qb qc >>= firstIntersection
        firstIntersection :: (Double, Double) -> Maybe Double
        firstIntersection (a, b)
            | b < 0     = Nothing
            | a < 0     = Just b
            | otherwise = Just a

instance I.Intersectable Sphere where
    intersect = sphereIntersect