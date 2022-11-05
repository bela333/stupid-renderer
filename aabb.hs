module AABB where
import Vector
import Intersectable as I
import Data.Maybe
data AABB = AABB Vec Vec

aabbUnion :: AABB -> AABB -> AABB
aabbUnion (AABB vmin1 vmax1) (AABB vmin2 vmax2) = AABB (vmin1 `vecMin` vmin2) (vmax1 `vecMax` vmax2)

aabbIntersection :: Vec -> Vec -> AABB -> Maybe Double
aabbIntersection ro (Vec rdx rdy rdz) (AABB vmin' vmax')
    | tmin > tmax = Nothing
    | tmin < 0 = Nothing
    | otherwise = Just tmin
    where
        Vec vminx vminy vminz = vecSubtract vmin' ro
        Vec vmaxx vmaxy vmaxz = vecSubtract vmax' ro
        x1 = vminx/rdx
        x2 = vmaxx/rdx
        y1 = vminy/rdy
        y2 = vmaxy/rdy
        z1 = vminz/rdz
        z2 = vmaxz/rdz
        xmin = x1 `min` x2
        xmax = x1 `max` x2
        ymin = y1 `min` y2
        ymax = y1 `max` y2
        zmin = z1 `min` z2
        zmax = z1 `max` z2
        tmin = xmin `max` ymin `max` zmin
        tmax = xmax `min` ymax `min` zmax


aabbMaxAxis :: AABB -> Axis
aabbMaxAxis (AABB vmin vmax) = vecMaxAxis $ vecSubtract vmax vmin

aabbMinAxis :: AABB -> Axis
aabbMinAxis (AABB vmin vmax) = vecMinAxis $ vecSubtract vmax vmin

data AABBWrapper a = AABBWrapper AABB a

instance Intersectable a => Intersectable (AABBWrapper a) where
    intersect ro rd (AABBWrapper aabb inner)
        | isJust $ aabbIntersection ro rd aabb = intersect ro rd inner
        | otherwise = Nothing