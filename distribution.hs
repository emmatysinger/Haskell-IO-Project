{-
    Write and submit a Haskell program (distribution.hs) that computes and displays 
    the distribution of characters in a given sample of text.
    
    Output of your program should look like this:
    
    Please enter a string of text (the bigger the better): 
    The rain in Spain stays mainly in the plain.
    The distribution of characters in "The rain in Spain stays mainly in the plain." is:
    iiiiii
    nnnnnn
    aaaaa
    sss
    ttt
    ee
    hh
    ll
    pp
    yy
    m
    r
    
    Notice about this example:
    * The text: 'The rain ... plain' is provided by the user as input to your program.
    * Uppercase characters are converted to lowercase
    * Spaces and punctuation marks are ignored completely.
    * Characters that are more common appear first in the list.
    * Where the same number of characters occur, the lines are ordered alphabetically. 
      For example, in the printout above, the letters e, h, l, p and y both occur twice 
      in the text and they are listed in the output in alphabetical order.
    * Letters that do not occur in the text are not listed in the output at all.
-}

import Data.Char (toLower)

count :: Eq a => a -> [a] -> Int
count a xs = length [x | x <-xs, x==a]

lengths :: [Char] -> [(Int,Char)]
lengths z = filter p (zip [count a z|a <-['a'..'z']] ['a'..'z'])
    where p x = fst x > 0

ordertuples :: (Ord a1, Ord a) => (a1, a) -> (a1, a) -> Bool
ordertuples a b
    |fst a > fst b = True
    |fst a < fst b = False
    |fst a == fst b && snd a < snd b = True
    |otherwise = False

quicksort :: (Ord a1, Ord a) => [(a1, a)] -> [(a1, a)]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, ordertuples x a]
        biggerSorted = quicksort [a | a <- xs, ordertuples a x]  
    in  biggerSorted ++ [x] ++ smallerSorted  

newlist :: [(Int, a)] -> [[a]]
newlist xs = map p xs
    where p x = replicate (fst x) (snd x)


main = do
    putStrLn "Please enter a string of text (the bigger the better): "
    string <- getLine
    let lower = map toLower string
        new = (newlist.quicksort.lengths) lower
    putStrLn $  "The distribution of characters in \"" ++ string ++ "\" is:"
    mapM_ putStrLn new
