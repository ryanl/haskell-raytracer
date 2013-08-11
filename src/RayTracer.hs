module RayTracer where

import Vector
import Intersect
import Image

type Light = (Vector, Colour)
type Scene = ([Surface], [Light])

black = [0, 0, 0]
bgcolour = [255, 255, 255]
dimensions = 3

zero_vector = Vector [0.0 | x <- [1..dimensions]]

-- Given a pixel position as a 2d vector, assuming the camera is at 0.0, 
-- generate the ray for that pixel. Pixel x and y co-ordinates should be in the
-- range [-0.5, 0.5]
ray_for_pixel :: Scalar -> Scalar -> Scalar -> Scalar -> Ray
ray_for_pixel    w         h         x         y      =  r
    where r         = Ray zero_vector direction
          direction = normalise (Vector [1.0, (x/w) - 0.5, (y/h) - 0.5])


-- Ray trace an entire scene.
-- Arguments: scene, width, height
-- Algorithm: for each pixel, work out what colour it should be independently
ray_trace :: Scene -> Integer -> Integer -> Image
ray_trace    scene    width  height  = (width, height, pixel_list)
    where pixel_list = [ get_colour_for_ray scene (ray_for_pixel w h x y) | 
                         y <- map fromInteger [0..height - 1], x <- map fromInteger [0..width - 1]]
          w :: Scalar
          w = fromInteger (width - 1)
          h :: Scalar
          h = fromInteger (height - 1)
          
-- Work out what surfaces the ray intersects and the distance at which it
-- first intersects.
-- Return value is a list of (surface, Just distance) or (surface, Nothing) 
get_ray_intersections :: Scene -> Ray -> [(Surface, [Scalar])]
get_ray_intersections    scene    ray =  intersections
    where intersections = [ (obj, ray_surface_intersect ray obj) | obj <- fst scene ]


-- To detect the colour of the ray, we find the first object it intersects 
-- (if any),
-- then we use the transparency, refractive index, reflectivity, emission
-- characterists of that object and some recursion upon rays that bounce off it
-- to determine a suitable colour
get_colour_for_ray :: Scene -> Ray -> Colour
get_colour_for_ray    scene    ray =  srdetectcolour' scene ray (srintersect scene ray)

srdetectcolour' :: Scene -> Ray -> Maybe (Surface, Scalar) -> Colour
srdetectcolour' scene (Ray rx rv) (Just (s,d)) = zipWith (+) lightadded (surface_colour s)
    where lightsvisible :: [Light]
          lightsvisible = lightsvisiblefrom intersectpoint scene
          lightadded :: Colour
          lightadded = (foldr (zipWith (+)) black . map effectivelight) lightsvisible
          effectivelight :: Light -> Colour
          effectivelight (v,c) = map (round . (*10000.0) . (/ (vector_sum ((intersectpoint - v) * (intersectpoint - v)))) . fromInteger) c
          intersectpoint = (rx + (mult d rv))
          
srdetectcolour' scene r Nothing      = bgcolour

lightsvisiblefrom :: Vector -> Scene -> [Light]
lightsvisiblefrom x scene@(surfs, lights) = filter isvis lights
    where isvis :: Light -> Bool
          isvis light = pastthelight (srintersect scene (Ray x (fst light - x)))
          pastthelight :: Maybe (Surface, Scalar) -> Bool
          pastthelight Nothing = True
          pastthelight (Just (s,d)) | d > 1 = True
                                    | otherwise = False

-- First intersection for the ray in the given scene
srintersect :: Scene -> Ray -> Maybe (Surface, Scalar)
srintersect scene ray = min_intersection(flatten_intersections (get_ray_intersections scene ray))

flatten_intersections :: [(Surface, [Scalar])] -> [(Surface, Scalar)]
flatten_intersections ((surf, []):extra) = flatten_intersections extra
flatten_intersections ((surf, xs):extra) = ((flatten_intersections' surf xs) ++ (flatten_intersections extra))
flatten_intersections [] = []
flatten_intersections' :: Surface -> [Scalar] -> [(Surface, Scalar)]
flatten_intersections' surf (x:xs) = (surf, x):(flatten_intersections' surf xs)
flatten_intersections' surf [] = []

min_intersection :: [(Surface, Scalar)] -> Maybe (Surface, Scalar)
min_intersection ((sa, a):(sb, b):extra) | a < b = min_intersection ((sa, a):extra)
                                         | otherwise = min_intersection ((sb, b):extra)
min_intersection [] = Nothing
min_intersection [(sa, a)] = Just (sa, a)
           
