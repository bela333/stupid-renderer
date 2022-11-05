module BVH where
import AABB
import qualified Intersectable as I
import Vector
import Triangle
import Data.List
data BVH = Node (AABBWrapper (I.IntersectablePair BVH BVH)) | Leaf (AABBWrapper Triangle)

bvhIntersect :: Vec -> Vec -> BVH -> Maybe I.Intersection
bvhIntersect ro rd (Node inner) = I.intersect ro rd inner
bvhIntersect ro rd (Leaf inner) = I.intersect ro rd inner

instance I.Intersectable BVH where
    intersect = bvhIntersect

constructBVH :: [Triangle] -> BVH
constructBVH [t] = Leaf $ wrappedTriangle $ t
constructBVH ts = Node $ AABBWrapper bb (I.IntersectablePair (constructBVH p1) (constructBVH p2))
    where
        thresholdIndex = (genericLength ts) `div` 2
        p1 = take thresholdIndex sorted
        p2 = drop thresholdIndex sorted
        sorted = sortBy compareCentroids ts
        axis = getAxis $ aabbMaxAxis bb
        compareCentroids c1 c2 = compare (axis $ triangleCentroid c1) (axis $ triangleCentroid c2)
        bb = meshBoundingBox ts
        AABB vmin vmax = bb