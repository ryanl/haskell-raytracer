module Image where

-- Join a list of strings using a separator string.
join :: String -> [String] -> String
 
join sep [x]    = x
join sep (x:xs) = x ++ sep ++ join sep xs


-- Test for "join".
test_join :: Bool
test_join =  join "t" ["12", "3", "", "xy"] == "12t3ttxy"


-- We'll (rather crudely) represent a colour by a list of integers, and an 
-- image by width, height and a list of colour samples.
-- This doesn't capture the fact that colours have 3 channels: red, green and
-- blue, or the fact that there will be exactly width*height colours, but it's
-- sufficient for our needs.
type Colour = [Integer]
type Image = (Integer, Integer, [Colour])


-- Create a string suitable for saving as a PPM file to represent an image.
create_ppm :: Image -> String
create_ppm (width, height, im) = join "\n" line_list ++ "\n"
    
    where line_list = (["P3", show width ++ " " ++ show height, "255"] ++
                        map (join " " . map show) im)                    
                      


-- Test for "create_ppm".
test_create_ppm :: Bool
test_create_ppm =  create_ppm (2, 3, [[11, 12, 13], [22, 23, 24],
                                      [33, 34, 35], [44, 45, 46],
                                      [55, 56, 57], [66, 67, 68]]) ==
                   "P3\n2 3\n255\n11 12 13\n22 23 24\n33 34 35\n44 45 46\n" ++
                   "55 56 57\n66 67 68\n"
                   
-- Produce a file "test0.ppm".
produce_sample_ppm = writeFile "test0.ppm" filedata
                     where mkcol x = [x,x,x]
                           filedata = create_ppm (16, 16, map mkcol [0..255])
