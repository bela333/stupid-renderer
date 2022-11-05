import qualified Data.ByteString.Lazy as B
import Data.Bits
import Data.Maybe
import Vector
import Sphere as S
import Triangle as T
import BVH as BVH
import qualified Intersectable as I
import AABB as AABB
import Model

-- Configuration data

data Conf a = Conf{
    width :: Integer,
    height :: Integer,
    light :: Vec,
    object :: a
}

epsilon = 0.001

-- Main rendering procedures

phongLighting :: I.Intersectable a => Conf a -> Vec -> Vec -> Vec -> Double
phongLighting Conf{object=object} light hit normal = (max 0 (vecDot toLight normal))
    where toLight = vecNormalise (vecSubtract light hit)

shadedPhong :: I.Intersectable a => Conf a -> Vec -> Vec -> Vec -> Double
shadedPhong conf@Conf{object=object} light hit normal = shaded intersection
    where
        phong = phongLighting conf light hit normal
        ro = vecAdd hit (vecMultiply normal epsilon)
        rd = vecNormalise $ vecSubtract light hit
        intersection = I.intersect ro rd object
        shaded :: Maybe I.Intersection -> Double
        shaded Nothing = phong
        shaded (Just I.Intersection{I.dist=dist})
            | dist > vecLength (vecSubtract light hit) = phong
            | otherwise                                = 0

--For quick access
shadingModel :: I.Intersectable a => Conf a -> Vec -> Vec -> Vec -> Double
shadingModel = shadedPhong

renderRay :: I.Intersectable a => Conf a -> Vec -> Vec
renderRay conf@Conf{light=light, object=object} rd = fromMaybe (Vec 0 0 0) (intersection >>= return . colorIntersection)
    where 
        colorIntersection :: I.Intersection -> Vec
        colorIntersection I.Intersection{I.color=color, I.normal=normal, I.pos=hit} = vecMultiply color (shadingModel conf light hit normal + 0.01)
        intersection = I.intersect (Vec 0 0 0) rd object

-- Rendering boilerplate

renderTexel :: I.Intersectable a => Conf a -> Integer -> Integer -> Vec
renderTexel conf@Conf{width=width, height=height} x y = renderMapped (fromIntegral x / widthF) (fromIntegral y / heightF)
    where
        widthF :: Double
        heightF :: Double
        widthF = fromIntegral width
        heightF = fromIntegral height
        aspect = widthF/heightF

        renderMapped :: Double -> Double -> Vec
        renderMapped x y = vecPow (renderRay conf $ vecNormalise $ Vec ((x-0.5)*aspect) (0.5-y) 1) (1.0/2.2)

renderImage :: I.Intersectable a => Conf a -> [[Vec]]
renderImage conf@Conf{width=width, height=height} = [[renderTexel conf x y | x <- [0..width-1]] | y <- [0..height-1]]

-- File writing

targaHeader :: I.Intersectable a => Conf a -> B.ByteString
targaHeader Conf{width=width, height=height} = B.pack [
    0,             -- ID length
    0,             -- Color map type
    2,             -- Image type: true-color image
    0, 0, 0, 0, 0, -- Color map specification
    0,0,0,0,       -- X and Y origin
    fromIntegral $ width .&. 255, fromIntegral $ (width `shiftR` 8) .&. 255,   -- Width
    fromIntegral $ height .&. 255, fromIntegral $ (height `shiftR` 8) .&. 255, -- Height
    24,32]

serializeImage :: I.Intersectable a => Conf a -> B.ByteString
serializeImage conf = B.pack $ map convertPixel $ concat $ concat $ (map.map) (vecToList.(\(Vec x y z) -> Vec z y x)) image
    where
        image = renderImage conf
        convertPixel x = round ((max (min x 1) 0)*255)


main :: IO ()
main = do
    triangles <- readObj "teapot.obj"
    let transformedTriangles = translateTriangles (Vec epsilon (-1) 5) $ scaleTriangles 0.5 triangles
    let teapot = constructBVH transformedTriangles
    --let conf = Conf{width=228, height=128, light=Vec 1 1 0, object=object}
    let conf = Conf{width=1920, height=1080, light=Vec 1 1 0, object=teapot}
    B.writeFile "output.tga" $ B.append (targaHeader conf) $ serializeImage conf