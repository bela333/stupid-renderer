module Plane where
import Intersectable as I
import Vector
data Plane = Plane Vec Double Vec

planeIntersect :: Vec -> Vec -> Plane -> Maybe I.Intersection
planeIntersect ro rd (Plane normal d color)
    | t < 0     = Nothing
    | otherwise = Just I.Intersection{
        color=color,
        dist=t,
        pos=hit,
        normal=normal
    }
    where
        t = (d-(vecDot normal ro))/(vecDot normal rd)
        hit = vecAdd ro (vecMultiply rd t)
        

instance I.Intersectable Plane where
    intersect = planeIntersect