module Main where

import Vector(Vector)
import Intersect(Sphere)
import Image(create_ppm, Colour, Image)

type Light = (Vector, Colour)

bgcolour   = [255,255,255]
zerovector = [0,0,0]


test2_scene = ([ Sphere (Vector [500.0, 0.0, 0.0]) 100.0 [50,70,90],
                Sphere (Vector [450.0, 100.0, 80.0]) 50.0 [250,30,10],
                Sphere (Vector [600.0, -100.0, -50.0]) 120.0 [200,220,250] ],
              [ (Vector [500.0, -200.0, -200.0], [200, 10, 10]),
                (Vector [300.0, 80.0, -100.0], [0, 40, 200]),
                (Vector [400.0, 150.0, -20.0], [60, 120, 60]) ])

test2 = create_ppm 2048 2048 (ray_trace test2_scene 2048 2048)

spacejoin (x:[]) = x
spacejoin (x:xs) = x ++ " " ++ spacejoin xs


-- fullscreenintersect :: Ray -> [Surface] -> Maybe Surface



main = writeFile "test2.ppm" test2
