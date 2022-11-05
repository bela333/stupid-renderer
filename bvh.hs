module BVH where
import AABB
import Intersectable as I
import Vector
import Triangle
import Data.List
data BVH = Node (AABBWrapper BVH) (AABBWrapper BVH) | Leaf (AABBWrapper Triangle)

bvhIntersect :: Vec -> Vec -> BVH -> Maybe I.Intersection
bvhIntersect ro rd (Node inner1 inner2) = extractIntersection intersection1 intersection2
    where
        intersection1 = I.intersect ro rd inner1
        intersection2 = I.intersect ro rd inner2
        extractIntersection :: Maybe I.Intersection -> Maybe I.Intersection -> Maybe I.Intersection
        extractIntersection Nothing Nothing = Nothing
        extractIntersection (Just a) Nothing = Just a
        extractIntersection Nothing (Just b) = Just b
        extractIntersection (Just a) (Just b)
            | (dist a) < (dist b) = Just a
            | otherwise           = Just b
bvhIntersect ro rd (Leaf inner) = I.intersect ro rd inner

instance I.Intersectable BVH where
    intersect = bvhIntersect

-- TODO: Use a better heuristic

constructBVH :: [Triangle] -> BVH
constructBVH [t] = Leaf $ wrappedTriangle $ t
constructBVH ts = Node (AABBWrapper bb1 $ constructBVH p1) (AABBWrapper bb2 $ constructBVH p2)
    where
        bb1 = meshBoundingBox p1
        bb2 = meshBoundingBox p2
        (p1, p2) = partition (\x -> axis (triangleCentroid x) > threshold) ts
        axis = getAxis $ aabbMaxAxis bb
        bb = meshBoundingBox ts
        AABB vmin vmax = bb
        threshold = ((axis vmin) + (axis vmax)) / 2