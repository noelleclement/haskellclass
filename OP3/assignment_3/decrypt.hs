module Main
    where

        import System.IO
        import Data.Char

        ------assignment decrypt------

        ---reads encrypted file
        ---reads key
        ---decrypts encrypted text
        ---writes decrypted text to new/separate file


        ----actual code----
        main = do
            text <- getTextFile
            key <- getKeyFile
            let decryptedText = decrypt text key
            writeFile "Decrypted.txt" decryptedText

        ---asks on screen what file needs to be encrypted
        ---reads the suggested(from input) file, and puts it in a monad (which is 'returned' from the function)
        getTextFile = do
            putStrLn "What is the name of the file that is wished to be decrypted?"
            input <- getLine 
            file <- readFile input          ---also can be put behind previous line with >>=, which is a bind
            return file                     ---return makes monad of file


        ---basically same as getTextFile, but for the keyfile
        getKeyFile = do
            putStrLn "What is the name of the key file?"
            input <- getLine 
            file <- readFile input         
            return file                    


        ---signature says it all (needed for decrypting > key)
        fromCharToInt :: [Char] -> [Int]
        fromCharToInt = map ((\x -> if x < 50 then x + 128 else x) . ord)

        ---other than above, bc meant for decrypting file itself
        unString :: [Char] -> [Int]
        unString [] = []
        unString (x:y:z:xs) = (read [x, y, z]):(unString xs)


        ---decrypting by changing text to ints, and changing key to ints
        ---subtract one from the other
        ---change the result to chars, which is put in list
        decrypt :: [Char] -> [Char] -> [Char]
        decrypt text key = map (chr . \x -> if x > 127 then x - 128 else x) $ zipWith (-) (unString text) (fromCharToInt key)