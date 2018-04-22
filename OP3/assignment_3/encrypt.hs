module Main
    where

        import System.IO
        import System.Random
        import Data.Char

        ---to compile: ghc encrypt.hs  or  ghc -o encrypt encrypt.hs
        ---to run: ./encrypt (or file name, or other name provided)



        -----------assignment encrypt program----------
        ---reads the textfile that needs to be encrypted
        ---generates key, based on randomgenarator
        ---encrypts file and writes encrypted file to new file
        ---writes the key in a separate .key file

        ---randomgenarator (System.Random > IO)
        ---read file (System.IO)
        ---encrypting (Data.Bits > Vernam algorithm)

        ---function that reads file and is monad
        ---function that genarates list with random values, of a given length, and is monad
        ---function for encrypting, not monad
        ---function that combines above 3 functions

       


        ---------- actual code --------------


        main = do
            text <- getTextFile
            gen <- getStdGen            --should other gen be wished, change here
            let key = genKey text gen
            let encryptedText = encrypt text key
            writeFile "Encrypted.text" encryptedText
            writeFile "Key.key" key



        ---asks on screen what file needs to be encrypted
        ---reads the suggested(from input) file, and puts it in a monad (which is 'returned' from the function)
        getTextFile = do
            putStrLn "What is the name of the file that is wished to be encrypted?"
            input <- getLine 
            file <- readFile input          ---also can be put behind previous line with >>=, which is a bind
            return file                     ---return makes monad of file

        

        ---generates list of 25 random characters
        ---makes a list as long as the text, with above list of chars, and put in monad
        genKey text gen = do                                       
            let randomChars = take 25 (randomRs ('a','z') gen)        
            key <- take (length text) (cycle randomChars)
            return key


        ---signature says it all
        fromCharToInt :: [Char] -> [Int]
        fromCharToInt = map ((\x -> if x < 50 then x + 128 else x) . ord)

         ---signature says it all (basically makes a string from them)
        fromIntToChar :: [Int] -> [Char]
        fromIntToChar = concat . map show

        
        ---text and key are converted to ints, 
        ---sommation of both, 
        ---convert them to string (char list)
        ---returns encrypted text (output from function)
        encrypt :: [Char] -> [Char] -> [Char]
        encrypt text key = fromIntToChar $ zipWith (+) (fromCharToInt text) (fromCharToInt key)




