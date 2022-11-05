module BVH where
import AABB
import Intersectable as I
import Vector
import Triangle
import Data.List
data BVH = Node (AABBWrapper BVHPair) | Leaf (AABBWrapper Triangle)
data BVHPair = BVHPair BVH BVH

bvhIntersect :: Vec -> Vec -> BVH -> Maybe I.Intersection
bvhIntersect ro rd (Node inner) = I.intersect ro rd inner
bvhIntersect ro rd (Leaf inner) = I.intersect ro rd inner

instance I.Intersectable BVH where
    intersect = bvhIntersect

bvhPairIntersect :: Vec -> Vec -> BVHPair -> Maybe I.Intersection
bvhPairIntersect ro rd (BVHPair bvh1 bvh2) = extractIntersection intersection1 intersection2
    where
        intersection1 = I.intersect ro rd bvh1
        intersection2 = I.intersect ro rd bvh2
        extractIntersection :: Maybe I.Intersection -> Maybe I.Intersection -> Maybe I.Intersection
        extractIntersection Nothing Nothing = Nothing
        extractIntersection (Just a) Nothing = Just a
        extractIntersection Nothing (Just b) = Just b
        extractIntersection (Just a) (Just b)
            | (dist a) < (dist b) = Just a
            | otherwise           = Just b

instance I.Intersectable BVHPair where
    intersect = bvhPairIntersect



constructBVH :: [Triangle] -> BVH
constructBVH [t] = Leaf $ wrappedTriangle $ t
constructBVH ts = Node $ AABBWrapper bb (BVHPair (constructBVH p1) (constructBVH p2))
    where
        thresholdIndex = (genericLength ts) `div` 2
        p1 = take thresholdIndex sorted
        p2 = drop thresholdIndex sorted
        sorted = sortBy compareCentroids ts
        axis = getAxis $ aabbMaxAxis bb
        compareCentroids c1 c2 = compare (axis $ triangleCentroid c1) (axis $ triangleCentroid c2)
        bb = meshBoundingBox ts
        AABB vmin vmax = bb