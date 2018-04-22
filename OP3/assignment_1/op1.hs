module Op1
    where

        import Data.List

        ---1: define datatype 'Geofig' , which is a geometric figure
        ----- algebraic - different values:
        --------- Square (param: Length)  - length is length of a side
        --------- Rectangle (param: Length, Width)
        --------- Triangle (param: Length) - equilateral
        --------- Circle (param: Radius)
        ----- color: Red, Blue, Yellow (which have an own datatype)

      
        data Geofig = Square { 
                        geocolour::Colour, 
                        geolength::Float 
                        }
                    | Rectangle { 
                        geocolour::Colour,
                        geolength::Float,
                        geowidth::Float 
                        }
                    | Triangle {
                        geocolour::Colour,
                        geolength::Float
                        } 
                    | Circle {
                        geocolour::Colour,
                        georadius::Float
                        }
                    deriving (Eq, Ord, Show)

        data Colour = Red
                    | Blue
                    | Yellow
                    deriving (Eq, Ord, Show)


        ---2: make some test figures
        testSquare = Square Blue 5
        testRectangle = Rectangle Yellow 2 8
        testTriangle = Triangle Red 4
        testCircle = Circle Yellow 11

        ---3: create a function that calculates the area of a given Geofig
        calcArea:: Geofig -> Float
        calcArea (Square _ l) = l^2
        calcArea (Rectangle _ l w) = l*w
        calcArea (Triangle _ l) = ((l^2)*(sqrt 3)) / 4
        calcArea (Circle _ r) = pi * (r^2)

        ---4: create a function that calculates the circumference of a given Geofig
        calcCirc:: Geofig -> Float
        calcCirc (Square _ l) = l*4
        calcCirc (Rectangle _ l w) = (l*2)+(w*2)
        calcCirc (Triangle _ l) = l*3
        calcCirc (Circle _ r) = pi * (r*2)

        

        ---5: create a function that gets a list of Geofigs and returns a list with only the Squares (and functions for the other Geofigs)
        listGeofigs = [testSquare, testRectangle, testTriangle, testCircle, Square Yellow 4]

        isSquare::Geofig -> Bool
        isSquare (Square _ _ ) = True
        isSquare _ = False

        findSquares:: [Geofig] -> [Geofig]              ---returned list cant be of Squares, because that isnt a datatype
        findSquares [] = []
        findSquares geofigs = filter isSquare geofigs

        isRectangle::Geofig -> Bool
        isRectangle (Rectangle _ _ _) = True
        isRectangle _ = False

        findRectangles:: [Geofig] -> [Geofig]             
        findRectangles [] = []
        findRectangles geofigs = filter isRectangle geofigs

        isTriangle::Geofig -> Bool
        isTriangle (Triangle _ _ ) = True
        isTriangle _ = False

        findTriangles:: [Geofig] -> [Geofig]             
        findTriangles [] = []
        findTriangles geofigs = filter isTriangle geofigs

        isCircle::Geofig -> Bool
        isCircle (Circle _ _ ) = True
        isCircle _ = False

        findCircles:: [Geofig] -> [Geofig]             
        findCircles [] = []
        findCircles geofigs = filter isCircle geofigs

        {- this 'works', but because only True possibility, it creates an exception
        https://stackoverflow.com/questions/20653830/filter-a-haskell-data-type
        findSquares:: [Geofig] -> [Geofig]             
        findSquares [] = []
        findSquares xs = filter (\ (Square _ _) -> True ) xs
        -}



        ---6: create a function that gets a String and a list of Geofigs and returns the geofigs that correspond with the String, like "Triangle"
        ------both versions work!
        findGeofigs::String -> [Geofig] -> [Geofig]
        findGeofigs _ [] = []
        findGeofigs "Square" geofigs = filter isSquare geofigs
        findGeofigs "Rectangle" geofigs = filter isRectangle geofigs
        findGeofigs "Triangle" geofigs = filter isTriangle geofigs
        findGeofigs "Circle" geofigs = filter isCircle geofigs

        findGeofigs2::String -> [Geofig] -> [Geofig]
        findGeofigs2 _ [] = []
        findGeofigs2 stringgeo geofigs
            |stringgeo=="Square" = filter isSquare geofigs
            |stringgeo=="Rectangle" = filter isRectangle geofigs
            |stringgeo=="Triangle" = filter isTriangle geofigs
            |stringgeo=="Circle"= filter isCircle geofigs


        ---7: create a function that gets a Colour and a list of Geofigs, return geofigs of that colour
        
        ---the lambda solution seems to work for functions that check for variables in data types
        findColours::Colour -> [Geofig] -> [Geofig]
        findColours _ [] = []
        findColours colour geofigs = filter (\g -> geocolour g==colour) geofigs         ---if colour is same as given colour, give true



        ---8: create a function that gets a list of Geofigs, returns geo with biggest area + one for biggest circumference
        -------not most elegant way, but it works!
        findBiggestArea::[Geofig] -> (Geofig, Float)
        findBiggestArea geofigs = maximum (zip geofigs (map calcArea geofigs))      ---calc the area for all elements, add those calcs to the list, find the max (=biggest area)
           
        findBiggestCirc::[Geofig] -> (Geofig, Float)
        findBiggestCirc geofigs = maximum (zip geofigs (map calcCirc geofigs))


        ---9: create a function that adds a Geofig to an existing list
        addGeofigToList::Geofig -> [Geofig] -> [Geofig]        ---so only possible with geofig lists, or do something with tuples or smth
        addGeofigToList list geofig = list:geofig               ---option 1, adds it to end of list
        ---addGeofigToList geofig list = insert geofig list       ---option 2, is "sorted" but i dont get how it is sorted tbh




        ---10: create a function that gets a list of objects, returns list with numbers. 
        ------each number is the % of area of the particular object, compared to the total area of all objects in the list
        ------thus: total of all numbers in list should be 100
        percAreaOfList::[Geofig] -> [Float]
        percAreaOfList geofigs = map percArea geofigs               ---apply percArea to all items of list
            where
                totArea = sum(map calcArea geofigs)                 ---sum of all the areas of the list
                percArea g = ((calcArea g) / totArea)*100           ---calculate the perc of that geofig, compared to totArea



