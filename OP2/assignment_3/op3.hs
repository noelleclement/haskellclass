module Op3
    where

        import Data.List

        ---1a: differentiating
        ----- f = function  - is a function that gets a double as input (x prob), and gives a double as output (y prob)
        ----- p = precision
        ----- x = point
        differentiate::(Double->Double)->Double->Double->Double
        differentiate f p x =
            let fx = f x                         --points x & x+p
                fxp = f (x+p)
            in ( fxp - fx ) / ( (x+p)-x )        --diff quotient 
        --put in prelude: func x = x^2 + 6*x +1
        --                differentiate func (1/10000) 3
            
        differentiateTest::Double->Double->Double
        differentiateTest p x =
            let func a = a^2 + 6*a + 1
                fx = func x
                fxp = func (x+p)
            in ( fxp - fx ) / ( (x+p)-x )

       
       
        





        ---1b: integrating
        ----- f = function
        ----- a b = interval [a,b]
        ----- p = precision (n)
        ----- riemann integral (left): sigma i=0>i=n-1 f(a + dx*i) * dx
        ----- khanacademy whoop: https://youtu.be/ViqrHGae7FA   
        integrate::(Double->Double)->Double->Double->Double->Double
        integrate f a b p = 
            let dx = (b-a)/p
            in foldr (\i r -> (dx * (f (a + (dx * i )))) + r) 0 [0..p-1]

        



        ---2: library
        ----- gets list as input, and gives the double characters as output, like:
        ----- ghci>doubles "aabbbcdeeeef"
        ----- ghci>"abe"

        ----- s \\ nub(s) checks whether there are duplicates in the list (nub(s)), then removes the first instances of that element
        ----- nub (...) then removes the duplicates of the above
        ----- Eq a => ... is needed for using \\
        doubles::Eq a => [a]->[a]
        doubles s = nub( s \\ nub(s) )





        ---3: poker! (higher order functions)

        
        throws = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s]
            where 
                s = [1..6]
        totalthrows = length (throws)

        --returns number of instances of c in list
        count::Integer->[Integer]->Integer
        count c [] = 0                  --when empty list is given, count = 0
        count c (x:xs)                  
            |c==x = 1 + (count c xs)    --if current element in list is c (what we're searching for), 1 +, and count through rest of list
            |otherwise = count c xs     --otherwise, count stays the same
               
        
        --creates a tuple with list of # of instances of 1,2...6, in a list (not a list in a list, so can't be used on throws)
        ----- ex: convert [1,2,2,2,5,5] > ([1,3,0,0,2,0],[1,2,2,2,5,5])
        ----- what IS possible: map convert throws
        convert list = ([a,b,c,d,e,f],list) 
            where
                a = count 1 list
                b = count 2 list
                c = count 3 list
                d = count 4 list
                e = count 5 list
                f = count 6 list



        --convert is mapped onto throws (only shows how many # of each element in each element (list) of throws)
        --show first elements of that new list 
        --filter the new list for elements (lists) with x # in it 
        amount x list = filter (elem x) (map fst (map convert throws))


        --find the throws in which 5 times the same number has been thrown
        poker = (fromIntegral (numberPoker) ) / (fromIntegral (totalthrows) )
            where
                numberPoker = length (listPoker)
                listPoker = amount 5 throws

        --find the throws in which 4 times the same number has been thrown
        fourOfAKind = (fromIntegral (numberFourOfAKind) ) / (fromIntegral (totalthrows) )
            where
                numberFourOfAKind = length(listFourOfAKind)
                listFourOfAKind = amount 4 throws

        --find the throws in which 3 times the same number has been thrown + 2 times another number
        fullHouse = (fromIntegral (numberFullHouse) ) / (fromIntegral (totalthrows) )
            where
                numberFullHouse = length(listFullHouse)
                listFullHouse = filter (elem 2) listThree       --find the throws with 3 times one number, but check whether another has been thrown 2 times
                listThree = amount 3 throws

        --find the throws in which 3 times the same number has been thrown (but filter the fullhouses)
        threeOfAKind = chanceThree - fullHouse
            where 
                chanceThree = (fromIntegral (numberThree) ) / (fromIntegral (totalthrows) )
                numberThree = length(listThree)
                listThree = amount 3 throws

        --find the throws with two pairs 
        twoPairs = (fromIntegral (numberTwoPairs) ) / (fromIntegral (totalthrows) )
            where
                numberTwoPairs = length(listTwoPairs)
                listTwoPairs = filter (elem 2) (map fst (map convert listPairs))        --instances with 2 x 2 throws (so the number of 2 throws should be 2 )
                listPairs = amount 2 throws                                             --instances with 2 throws of a number

        --to find pairs, search for instances with two throws, but filter the twopairs and fullhouses out (which both have instances of a pair)
        pairs = chancePair - twoPairs - fullHouse
            where
                chancePair = (fromIntegral (numberPairs) ) / (fromIntegral (totalthrows) )
                numberPairs = length(listPairs)
                listPairs = amount 2 throws

        --have 5 throws with all a different number 
        straight = (fromIntegral (numberStraight) ) / (fromIntegral (totalthrows) )
            where 
                numberStraight = length(listStraight)
                listStraight = listFiveOnes
                listFiveOnes = filter (elem 5) (map fst (map convert listOnes))     --this checks whether there have been 5 throws of a different number (doesn't take into account whether it's in order)
                listOnes = amount 1 throws
        --actually want to create a function where it iterates through listOnes, to see whether the head or tail has zero -> makes it a straight, but didn't get it to work

        --all that's left!
        bust = 1 - poker - fourOfAKind - fullHouse - threeOfAKind - twoPairs - pairs - straight












        