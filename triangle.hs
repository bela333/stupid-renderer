module Triangle where
import Vector
import AABB
import qualified Intersectable as I
data Triangle = Triangle (Vec, Vec, Vec) Vec


triangleIntersect :: Vec -> Vec -> Triangle -> Maybe I.Intersection
triangleIntersect ro rd (Triangle (a, b, c) color) = cramer (ca, cb, cc) cd >>= checkBounds >>= Just . resultToIntersection
    where
        ca = vecSubtract c a
        cb = vecSubtract c b
        cc = rd
        cd = vecSubtract c ro
        checkBounds :: Vec -> Maybe Vec
        checkBounds vec@(Vec u v t)
            | u < 0 || v < 0 || 1 < u+v || u > 1 || v > 1 || u+v <= 0 || t < 0 = Nothing
            | otherwise = Just vec
        resultToIntersection :: Vec -> I.Intersection
        resultToIntersection (Vec u v t) = I.Intersection{
            I.color = color,
            I.pos = vecAdd ro (vecMultiply rd t),
            I.dist = t,
            I.normal = vecNormalise $ vecCross (vecSubtract a c) (vecSubtract b c)
        }

triangleBoundingBox :: Triangle -> AABB
triangleBoundingBox (Triangle (a, b, c) _) = AABB vmin vmax
    where
        vmin = a `vecMin` b `vecMin` c
        vmax = a `vecMax` b `vecMax` c

meshBoundingBox :: [Triangle] -> AABB
meshBoundingBox ts = foldl aabbUnion x xs
    where
        (x:xs) = map triangleBoundingBox ts

triangleCentroid :: Triangle -> Vec
triangleCentroid (Triangle (a, b, c) _) = vecDivide (a `vecAdd` b `vecAdd` c) 3

wrappedTriangle :: Triangle -> AABBWrapper Triangle
wrappedTriangle t = AABBWrapper (triangleBoundingBox t) t

scaleTriangle :: Double -> Triangle -> Triangle
scaleTriangle f (Triangle (a, b, c) color) = Triangle (scale a, scale b, scale c) color
    where scale = (`vecMultiply` f)

scaleTriangles :: Double -> [Triangle] -> [Triangle]
scaleTriangles f = map (scaleTriangle f)

translateTriangle :: Vec -> Triangle -> Triangle
translateTriangle transf (Triangle (a, b, c) color) = Triangle (a `vecAdd` transf, b `vecAdd` transf, c `vecAdd` transf) color

translateTriangles :: Vec -> [Triangle] -> [Triangle]
translateTriangles transf = map (translateTriangle transf)

instance I.Intersectable Triangle where
    intersect = triangleIntersect