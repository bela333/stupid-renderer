module Model where
import Vector
import Triangle
import Data.List

readVertices :: [[String]] -> [Vec]
readVertices lines = map lineToVector $ filter (isVertex) lines
    where
        isVertex (x:xs) = x == "v"
        isVertex [] = False
        lineToVector :: [String] -> Vec
        lineToVector (_:x:y:z:_) = Vec (read x) (read y) (read z)

readFaces :: [[String]] -> [Vec] -> Vec -> [Triangle]
readFaces lines vertices color = map lineToFace $ filter (isFace) lines
    where
        isFace (x:xs) = x == "f"
        isFace [] = False
        extractVertexIndex :: String -> String
        extractVertexIndex xs = takeWhile (/= '/') xs
        lineToFace :: [String] -> Triangle
        lineToFace (_:vi1:vi2:vi3:_) = Triangle (v1, v2, v3) color
            where
                v1 = vertices `genericIndex` (read (extractVertexIndex vi1) - 1)
                v2 = vertices `genericIndex` (read (extractVertexIndex vi2) - 1)
                v3 = vertices `genericIndex` (read (extractVertexIndex vi3) - 1)

swapDirections :: Vec -> Vec
swapDirections (Vec x y z) = Vec x z y

swapDirectionsOnTriangle :: Triangle -> Triangle
swapDirectionsOnTriangle (Triangle (a, b, c) color) = Triangle (swapDirections a, swapDirections b, swapDirections c) color

readObj :: FilePath -> Vec -> IO [Triangle]
readObj path color = do
    content <- readFile path
    let parts = map words $ lines content
    let vertices = readVertices $ parts
    let faces = readFaces parts vertices color
    return faces