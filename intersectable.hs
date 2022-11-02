module Intersectable where
import Vector

data Intersection = Intersection{
    color :: Vec,
    normal :: Vec,
    pos :: Vec,
    dist :: Double
}

class Intersectable a where
    intersect :: Vec -> Vec -> a -> Maybe Intersection

listIntersect :: Intersectable a => Vec -> Vec -> [a] -> Maybe Intersection
listIntersect _ _ [] = Nothing
listIntersect ro rd [x] = intersect ro rd x
listIntersect ro rd (x:xs) = extractIntersection a b
    where
        a = intersect ro rd x
        b = listIntersect ro rd xs
        extractIntersection :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
        extractIntersection Nothing Nothing = Nothing
        extractIntersection (Just a) Nothing = Just a
        extractIntersection Nothing (Just b) = Just b
        extractIntersection (Just a) (Just b)
            | dist a < dist b = Just a
            | otherwise       = Just b

instance Intersectable a => Intersectable [a] where
    intersect = listIntersect