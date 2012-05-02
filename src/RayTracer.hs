module RayTracer where

type Scene = ([Surface], [Light])


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
ray_trace :: Scene -> Int -> Int -> Image
ray_trace    scene    width  height  =  pixel_list
    where pixel_list = [ get_colour_for_ray scene (ray_for_pixel w h x y) | 
                         y <- [0..height - 1], x <- [0..width - 1]]
          w :: Scalar
          w = fromInteger (width - 1)
          h :: Scalar
          h = fromInteger (height - 1)
          
-- Work out what surfaces the ray intersects and the distance at which it
-- first intersects.
-- Return value is a list of (surface, Just distance) or (surface, Nothing) 
get_ray_intersections :: Scene -> Ray -> [(Surface, Maybe Scalar)]
get_ray_intersections    scene    ray =  intersections
    where intersections = [ (obj, intersect ray obj) | obj <- fst scene ]


-- To detect the colour of the ray, we find the first object it intersects 
-- (if any),
-- then we use the transparency, refractive index, reflectivity, emission
-- characterists of that object and some recursion upon rays that bounce off it
-- to determine a suitable colour
get_colour_for_ray :: Scene -> Ray -> Colour
get_colour_for_ray    scene    ray =  srdetectcolour' scene ray (srintersect scene ray)

srdetectcolour' :: Scene -> Ray -> Maybe (Surface, Scalar) -> Colour
srdetectcolour' scene (Ray rx rv) (Just (s,d)) = zipWith (+) lightadded (surfcolour s)
    where lightsvisible :: [Light]
          lightsvisible = lightsvisiblefrom intersectpoint scene
          lightadded :: Colour
          lightadded = (foldr (zipWith (+)) zerocolour . map effectivelight) lightsvisible
          effectivelight :: Light -> Colour
          effectivelight (v,c) = map (round . (*10000.0) . (/ (vectorsum ((intersectpoint - v) * (intersectpoint - v)))) . fromInteger) c
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

                       
srintersect :: Scene -> Ray -> Maybe (Surface, Scalar)
srintersect scene ray = fstintersect Nothing (srintersections scene ray)
    where fstintersect Nothing ((s, Nothing):xs) = fstintersect Nothing xs
          fstintersect Nothing ((s, Just x):xs) = fstintersect (Just (s,x)) xs
          fstintersect m ((s, Nothing):xs) = fstintersect m xs
          fstintersect (Just (ms, m)) ((s, Just x):xs) | m > x = fstintersect (Just (s,x)) xs
                                                       | otherwise = fstintersect (Just (ms, m)) xs
          fstintersect m [] = m


