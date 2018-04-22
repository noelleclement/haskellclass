module Op2
    where 

        import Data.Char


        ---1a: calculates greatest common divider, for 2 given natural numbers x and y ---
        euclid::Integer->Integer->Integer       
        euclid x 0 = x                          --if result == 0, the gcd is the mod of the 'previous' calculation
        euclid x y = euclid y (x `mod` y)       --if given 2 natural numbers, calculate euclid again with the smallest number (new biggest number), and the modulo of the 2 numbers





        ---1b: rewrite/use below algorithm for generating key, so it doesn't give negative results ( e · d = 1(mod m) )---
        egcd::Integer -> Integer -> (Integer,Integer,Integer)
        egcd 0 b = (b, 0, 1)
        egcd a b =
            let (g, s, t) = egcd (b `mod` a) a
            in (g, t - (b `div` a) * s, s)
        --egcd a b > 
        ----- a = e, 
        ----- b = m

        ---outcome:
        ----- (gcd, start (1?)+modulo(b) divided by e, how many times divided? > 'steps')


        ownEgcd::Integer -> Integer ->(Integer,Integer,Integer)
        ownEgcd a b
            |mid <0 = let (x, y, z) = oldEgcd
                        in (x, y+b, z)              --b is the modulo
            |otherwise = oldEgcd 
            where 
                oldEgcd = egcd a b
                mid = middle(oldEgcd)
        
        middle (_,b,_) = b 
                   
        --I didn't want to just rewrite the old algorithm, but this doesn't seem to fully solve the problem
        --so, what isn't handled in this + notes:
        -----it is expected that a is e, and b is the modulo
        -----it only changes the negative 'middle' outcomes, which is d (see outcome of egcd for explanation)
        -----so: if the order changes due to other input, may give faulty result
        -----does adding the modulo really solve the 'negative outcome' problem?      






        ---2: key generation---

        --a function to check whether an Integer is a prime number
        prime:: Integer -> Bool
        prime n = (n>1) && all (\ x -> mod n x /= 0) [2..n-1]         --if n is above 1, and for all the numbers between 2 and n-1 the modulo isn't 0, then it's prime
        --if asking for input, use: [x|x<-[100..500],prime x]

        --but for now i'll choose 2 prime numbers myself:
        p=271
        q=467


        --modulo
        m = p*q    --126.557

        --Euleurs totient function
        m' = (p-1)*(q-1)      --125.820

        --choose e
        ---- e<m'
        ---- gcd(e, m') = 1
        ---- [x|x<-[1..m'],gcd x m' == 1]
        e = 99457

        --calculate d ( e · d = 1(mod m') )
        --1(mod m')/e?
        d = middle (ownEgcd e m')

        --e = private key
        --d = public key
        --m = modulo





        ---3a: RSA encryption
        -----gets tuple of key and modulo, and a number as parameter, and returns encrypted number
        -----C = x^e (mod m)
        rsaencrypt::(Integer,Integer)->Integer->Integer
        rsaencrypt (e,m) x = x^e `mod` m


        ---3b: RSA decryption
        ------gets tuple of key and modulo, and a number as parameter, and returns decrypted number
        rsadecrypt::(Integer,Integer)->Integer->Integer
        rsadecrypt (e,m) x = x^e `mod` m





        ---4: encrypt and decrypt one character with 3a/3b functions
        ----- convert to ascii with ord (char to num) and chr (num to char) functions
        nOrd = (ord 'n')
        nChr = (chr nOrd)


        ---5: how to use RSA for securely communicate
        ---Q:Alice encrypts message with private key, but anyone can decrypt with public key. How can Bob and Alice communicate safely
        ---A:Either:
            --Alice uses public key for encrypting, and private for decrypting (which she gives to Bob)
            --Alice does the above, but also adds Bob's private or public key, so both can be sure that noone except them can encrypt and decrypt






        -- this is very sad, but just in case I forget again how to run interpreted mode: ghci op2.hs