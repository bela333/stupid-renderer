import qualified Data.ByteString.Lazy as B
import Data.Bits
import Data.Maybe
import Vector
import Sphere as S
import qualified Intersectable as I

data Conf = Conf{
    width :: Integer,
    height :: Integer
}

targaHeader :: Conf -> B.ByteString
targaHeader Conf{width=width, height=height} = B.pack [
    0,             -- ID length
    0,             -- Color map type
    2,             -- Image type: true-color image
    0, 0, 0, 0, 0, -- Color map specification
    0,0,0,0,       -- X and Y origin
    fromIntegral $ width .&. 255, fromIntegral $ (width `shiftR` 8) .&. 255,   -- Width
    fromIntegral $ height .&. 255, fromIntegral $ (height `shiftR` 8) .&. 255, -- Height
    24,32]


mySphere = S.Sphere{
    origin=Vec 0 0 2,
    radius=0.5,
    color=Vec 1 1 1
}

light = Vec 1 1 0

phongLighting :: Vec -> Vec -> Double
phongLighting hit normal = (max 0 (vecDot toLight normal)) + 0.01
    where toLight = vecNormalise (vecSubtract light hit)

renderRay :: Vec -> Vec
renderRay rd = fromMaybe (Vec 0 0 0) (intersection >>= return . colorIntersection)
    where 
        colorIntersection :: I.Intersection -> Vec
        colorIntersection I.Intersection{I.color=color, I.normal=normal, I.pos=hit} = vecMultiply color (phongLighting hit normal)
        intersection = I.intersect (Vec 0 0 0) rd mySphere


renderTexel :: Conf -> Integer -> Integer -> Vec
renderTexel Conf{width=width, height=height} x y = renderMapped (fromIntegral x / widthF) (fromIntegral y / heightF)
    where
        widthF :: Double
        heightF :: Double
        widthF = fromIntegral width
        heightF = fromIntegral height
        aspect = widthF/heightF

        renderMapped :: Double -> Double -> Vec
        renderMapped x y = vecPow (renderRay $ vecNormalise $ Vec ((x-0.5)*aspect) (0.5-y) 1) (1.0/2.2)

renderImage :: Conf -> [[Vec]]
renderImage conf@Conf{width=width, height=height} = [[renderTexel conf x y | x <- [0..width-1]] | y <- [0..height-1]]

serializeImage :: Conf -> B.ByteString
serializeImage conf = B.pack $ map convertPixel $ concat $ concat $ (map.map) (vecToList.(\(Vec x y z) -> Vec z y x)) image
    where
        image = renderImage conf
        convertPixel x = round ((max (min x 1) 0)*255)

conf = Conf{width=128, height=128}

main :: IO ()
main = do
    B.writeFile "output.tga" $ B.append (targaHeader conf) $ serializeImage conf