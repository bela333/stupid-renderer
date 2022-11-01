import qualified Data.ByteString.Lazy as B
import Data.Bits
import Vector

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

renderTexel :: Conf -> Integer -> Integer -> Vec
renderTexel Conf{width=width, height=height} x y = renderMapped (fromIntegral x / fromIntegral width) (fromIntegral y / fromIntegral height)
    where
        renderMapped :: Double -> Double -> Vec
        renderMapped x y = Vec x y (x*y)

renderImage :: Conf -> [[Vec]]
renderImage conf@Conf{width=width, height=height} = [[renderTexel conf x y | x <- [0..width-1]] | y <- [0..height-1]]

serializeImage :: Conf -> B.ByteString
serializeImage conf = B.pack $ map convertPixel $ concat $ concat $ (map.map) vecToList image
    where
        image = renderImage conf
        convertPixel x = round ((max (min x 1) 0)*255)

conf = Conf{width=128, height=128}

main :: IO ()
main = do
    B.writeFile "output.tga" $ B.append (targaHeader conf) $ serializeImage conf