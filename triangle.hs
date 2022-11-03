module Triangle where
import Vector
import qualified Intersectable as I
data Triangle = Triangle (Vec, Vec, Vec) Vec

triangleIntersect :: Vec -> Vec -> Triangle -> Maybe I.Intersection
triangleIntersect ro rd (Triangle (a, b, c) color) = cramer ca cb cc cd >>= checkBounds >>= Just . resultToIntersection
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

instance I.Intersectable Triangle where
    intersect = triangleIntersect