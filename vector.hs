module Vector where
data Vec = Vec Double Double Double

getX :: Vec -> Double
getX (Vec x y z) = x

getY :: Vec -> Double
getY (Vec x y z) = y

getZ :: Vec -> Double
getZ (Vec x y z) = z

vecToList :: Vec -> [Double]
vecToList (Vec a b c) = [a, b, c]