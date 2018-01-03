module Op1
    where 

        ----1a: calculates factorial and uses pattern matching(recursive)----
        faca::Int->Int
        faca 0 = 1
        faca x = x * faca (x-1)

        ----1b: calculates factorial and uses guards---
        facb::Int->Int
        facb x
            | x<=0 = 1
            | x>0 = x * facb (x-1)


        ---2a: calculates zeropoints of 2nd grade function---

        --just the abcformula (with 2 returned list elements since 2 possible results from abcformula)
        --abc formula: x = (-b +- sqrt(b^2 - 4*a*c))/2*a
        abcformulaa::Double->Double->Double->[Double]
        abcformulaa a b c = [(-b + sqrt(b^2 - 4.0*a*c))/2.0*a, (-b - sqrt(b^2 - 4.0*a*c))/2.0*a]

        --use abcformula to calculate zeropoints---
        --return of zeropointsa a b c = [ [x1, y1] , [x2, y2] ]--
        --returns list of list (with Doubles) because /\
        zeropointsa::Double->Double->Double->[[Double]]
        zeropointsa a b c = [ [abcformulaa a b c !! 0, a * abcformulaa a b c !! 0 ^2 + b * abcformulaa a b c !! 0 + c] , [abcformulaa a b c !! 1, a * abcformulaa a b c !! 1 ^2 + b * abcformulaa a b c !! 1 + c] ]




        ---2b: calculates zeropoints of 2nd grade function, using where and guards---

        --abc formula with added D reference
        abcformulab::Double->Double->Double->[Double]
        abcformulab a b c
            | d<0 = []                                              --if Determinant is below 0, no solution, so return list of 2 empty lists
            | d==0 = [-b/2*a]                                       --if Determinant is 0, only 1 solution, so return list with solution + empty list
            | d>0 = [(-b + sqrt(d))/2.0*a, (-b - sqrt(d))/2.0*a]    --if Determinant is above 0, 2 solutions, so return list with 2 solutions
            where
                d = b^2 - 4.0*a*c                                   --Determinant declared

        --zeropoints which looks at the amount of results from abcformula to adapt the returned list
        zeropointsb::Double->Double->Double->[[Double]]
        zeropointsb a b c
            | lengthlist==0 = [ [] , [] ]                           --if abc has no elements (d<0), return list with 2 empty list
            | lengthlist==1 = [ [x1,y1] , [] ]                      --if abc has 1 element (d==0), return list with first coordinates and empty list
            | lengthlist>1 = [ [x1,y1] , [x2,y2] ]                  --if abc has 2 elements (d>0), return list with bothcoordinates  

            where 
                abc = abcformulab a b c                             --shortcut for call of abcformula[]
                lengthlist = length abc                             --shortcut for checking length of abcformula[]
                x1 = abc !! 0                                       --x1 is first element of abcformula[]
                y1 = a * x1^2 + b * x1 + c                          --y1 is calculated with x1 (corresponding y coordinate), with 2nd grade function setup
                x2 = abc !! 1                                       --x2 is second element of abcformula[]
                y2 = a * x2^2 + b * x2 + c                          --y2 is calculated with x2 (corresponding y coordinate), with 2nd grade function setup



        ---2c: list of all possible throws with 3 dices + aremultiples of 5)
        throwndice::Int                                                                      --only returns Int, no parameters since no input necessary
        throwndice = length dicelist                                                         --only returns the length of the calculated amount of results
            where 
                dicelist = [(x,y,z) | x<-[1..6], y<-[1..6], z<-[1..6], (x+y+z) `mod` 5==0]   --the actual list of dice results, 3 dices (x,y,z), mod for calculating whether sum of dice multiple of 5


        ---2d: 2c + modulo of n
        --same as 2c but an input is added (n), which checks how many multiples of n are in the results of sum of the dice
        throwndice'::Int->Int
        throwndice' n = length dicelist
            where
                dicelist = [(x,y,z) | x<-[1..6], y<-[1..6], z<-[1..6], (x+y+z) `mod` n==0]




