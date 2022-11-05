module Vector where
data Vec = Vec Double Double Double deriving (Show)
data Axis = X | Y | Z

getAxis :: Axis -> Vec -> Double
getAxis X (Vec x y z) = x
getAxis Y (Vec x y z) = y
getAxis Z (Vec x y z) = z

getX :: Vec -> Double
getX (Vec x y z) = x

getY :: Vec -> Double
getY (Vec x y z) = y

getZ :: Vec -> Double
getZ (Vec x y z) = z

vecToList :: Vec -> [Double]
vecToList (Vec a b c) = [a, b, c]

vecDot :: Vec -> Vec -> Double
vecDot (Vec x y z) (Vec a b c) = a*x + b*y + c*z

vecMultiply :: Vec -> Double -> Vec
vecMultiply (Vec x y z) c = Vec (x*c) (y*c) (z*c)

vecPow :: Vec -> Double -> Vec
vecPow (Vec x y z) c = Vec (x**c) (y**c) (z**c)

vecDivide :: Vec -> Double -> Vec
vecDivide v c = vecMultiply v (1/c)

vecAdd :: Vec -> Vec -> Vec
vecAdd (Vec x y z) (Vec a b c) = Vec (x+a) (y+b) (z+c)

vecSubtract :: Vec -> Vec -> Vec
vecSubtract (Vec x y z) (Vec a b c) = Vec (x-a) (y-b) (z-c)

vecNegate :: Vec -> Vec
vecNegate (Vec x y z) = Vec (-x) (-y) (-z)

vecLengthSquared :: Vec -> Double
vecLengthSquared a = a `vecDot`a

vecLength :: Vec -> Double
vecLength a = sqrt $ a `vecDot`a

vecNormalise :: Vec -> Vec
vecNormalise v = vecDivide v (vecLength v)

determinant :: Vec -> Vec -> Vec -> Double
determinant (Vec x1 y1 z1) (Vec x2 y2 z2) (Vec x3 y3 z3) = x1*p1 - y1*p2 + z1*p3
    where
        p1 = y2*z3-y3*z2
        p2 = x2*z3-x3*z2
        p3 = x2*y3-x3*y2

cramer :: Vec -> Vec -> Vec -> Vec -> Maybe Vec
cramer a b c d 
    | det == 0  = Nothing
    | otherwise = Just $ vecDivide innerVec det
    where
        det = (determinant a b c)
        innerVec = Vec (determinant d b c) (determinant a d c) (determinant a b d)

vecCross :: Vec -> Vec -> Vec
vecCross (Vec a1 a2 a3) (Vec b1 b2 b3) = Vec d e f
    where
        d = a2*b3 - a3*b2
        e = a3*b1 - a1*b3
        f = a1*b2 - a2*b1

vecMin :: Vec -> Vec -> Vec
vecMin (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 `min` x2) (y1 `min` y2) (z1 `min` z2)

vecMax :: Vec -> Vec -> Vec
vecMax (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 `max` x2) (y1 `max` y2) (z1 `max` z2)

vecMaxAxis :: Vec -> Axis
vecMaxAxis (Vec x y z)
    | x > y && x > z = X
    | y > x && y > z = Y
    | otherwise      = Z

vecMinAxis :: Vec -> Axis
vecMinAxis (Vec x y z)
    | x < y && x < z = X
    | y < x && y < z = Y
    | otherwise      = Z