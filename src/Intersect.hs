-- Intersect module is used to find where lines intersect objects
module Intersect where

import Vector
import Image

-- other visual properties to consider:
-- refractive index, transparency

-- Sphere (centre position) (radius) (surface colour)
data Surface = Sphere Vector Scalar Colour


-- (Ray x v) represents the set { x + d*v : v >= 0 }
-- v should have length 1
data Ray = Ray Vector Vector

data Intersection = Intersection Vector Vector

surface_colour :: Surface -> Colour
surface_colour (Sphere x r c) = c


epsilon :: Scalar
epsilon =  0.0000001

-- solve_quadratic gives the solutions to the given quadratic equation
-- ax^2 + bx + c == 0

solve_quadratic :: Scalar -> Scalar -> Scalar -> [Scalar]

solve_quadratic    a         b         c         =  solutions

    where solutions | discriminant < 0 = []
                     | otherwise        = [(-b + sqrt_d) / (2.0 * a),
                                           (-b - sqrt_d) / (2.0 * a)]
          discriminant                  = (b * b) - (4.0 * a * c)
          sqrt_d                        = sqrt discriminant 


-- ray_surface_intersect returns a list of the values of d where
-- Ray rx rv intersects the surface at rx+(d*rv).
ray_surface_intersect :: Ray -> Surface -> [Scalar]

-- Case for spherical surfaces
ray_surface_intersect (Ray rx rv) (Sphere sx radius col) = intersections
    where intersections = filter (> epsilon) (solve_quadratic a b c)
                           -- we don't to see out of the back of our head!
          se = (sx - rx)
          b = (-2.0) * vector_sum (rv * se)
          a = vector_sum (rv * rv)
          c = vector_sum (se * se) - (radius * radius)

test_intersect = abs(a - 57.7350269189626) < 0.000001
    where (a:as) = (ray_surface_intersect
                         (Ray (Vector [0.0,0.0,0.0]) (Vector [1.0,1.0,1.0]))
                         (Sphere (Vector [0.0,0.0,0.0]) 100.0 [50,50,50]))

