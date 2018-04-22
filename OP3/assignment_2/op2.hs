module Op2
    
    where 

        import Data.List

        ---1: create data type 'book' with: price, title & author

        data Book = Book Price Title Author
            deriving (Show)

        type Price = Int
        type Title = String
        type Author = String


        ---2: create Eq and Ord instances, so the books can be compared & ordered/sorted
        instance Eq Book where
            (Book price1 title1 author1) == (Book price2 title2 author2) = (price1==price2) && (title1==title2) && (author1==author2)
            _ /= _ = False

        instance Ord Book where
            compare (Book _ title1 _) (Book _ title2 _) = compare title1 title2


        ---3: make a list of at least 5 books (+test eq & ord instances with them)
        book1 = Book 20 "Ready Player One" "Ernest Cline"
        book2 = Book 15 "Throne of Glass" "Sarah J Maas"
        book3 = Book 13 "Court of Thorns and Roses" "Sarah J Maas" 
        book4 = Book 5 "The Giver" "Lois Lowry"
        book5 = Book 9 "The Sun and Her Flowers" "Rupi Kaur"

        bookList = [book1, book2, book3, book4, book5]

        ---book1==book2 > False
        ---book1==book1 > True
        ---sort bookList gives sorted list ('The' is seen as the word to compare with first)



        ---4: create data type 'box', which can be empty or has 'something' (a)
        data Box a = EmptyBox
                | Box a 
                deriving (Show)


        ---5: create a function that puts all books from a list in a box + the other way around
        putBooklistInBox:: [Book] -> Box [Book]
        putBooklistInBox booklist = Box booklist

        getBooklistFromBox:: Box [Book] -> [Book]
        getBooklistFromBox (Box booklist) = booklist

        ------------------------------------------------------------------------

        ---6: create data type 'bag', like box
        ---7 (5.2): add functor to both Box and Bag
       
        data Bag a = EmptyBag
                | Bag a
                deriving (Show)
       

        instance Functor Box where
            fmap f EmptyBox = EmptyBox
            fmap f (Box a) = Box (f a)

        instance Functor Bag where
            fmap f EmptyBag = EmptyBag
            fmap f (Bag a) = Bag (f a)

        ---8 (6.2): create the following functions:
        ------all books in a booklist are separately put in a bag, and all bags are seperately put in a box
        ------create a list with random numbers, put every number in a box 

        putBooksInBagsAndBoxes:: [Book] -> [Box (Bag Book)]                             ---can this be done with Applicative?
        putBooksInBagsAndBoxes booklist =  map (fmap inBag) boxedList                   ---what is in the boxes will be put in a bag (bc of that 'weird order of mapping')
                where
                    inBag a = Bag a
                    inBox a = Box a
                    boxedList = map inBox booklist
        
        numberList = [4, 6, 3, 7, 7, 8, 18, 21, 1, 5]

        putNumbersInBoxes:: Num a => [a] -> [Box a] 
        putNumbersInBoxes numlist = map inBox numlist           ---no fmap
                where 
                    inBox a = Box a



        ---9 (7): create data type 'List' 
        ------ parametered, defines a list recursively, functor
        ------ fill List with boxes with numbers (foldr/foldl)
        ------ use Functor to write function which makes of a List filled with Boxes, a List filled with Bags

        data List a = EmptyList 
                | Head a (List a)
                deriving (Show)

        
        
        instance Functor List where
            fmap f EmptyList = EmptyList
            fmap f (Head a tail) = Head (f a) (fmap f tail)        

        push :: a -> List a -> List a
        push a EmptyList = Head a EmptyList
        push a (Head h tail) = Head a (Head h tail)

        fillList lijst = foldr push EmptyList lijst 
    
        convertBoxToBag:: Box a -> Bag a
        convertBoxToBag (Box a) = Bag a

        replaceBoxes = fmap convertBoxToBag fillList


        ---10 (8): create a tree like structure, in the same way as done in 9

        data Tree a = EmptyTree
                    | Node a (Tree a) (Tree a)
                    deriving (Show)

        instance Functor Tree where
            fmap f EmptyTree       = EmptyTree
            fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2) 

        testTree = Node 1 (Node 1 (Node 3 EmptyTree EmptyTree) EmptyTree) (Node 2 EmptyTree (Node 4 (Node 1 EmptyTree EmptyTree) EmptyTree))


