module Intersectable where
import Vector

data Intersection = Intersection{
    color :: Vec,
    normal :: Vec,
    pos :: Vec
}

class Intersectable a where
    intersect :: Vec -> Vec -> a -> Maybe Intersection