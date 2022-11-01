module Vector where
data Vec = Vec Double Double Double deriving (Show)

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