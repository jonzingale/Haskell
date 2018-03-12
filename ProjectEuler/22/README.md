
### Names scores

Using names.txt, a file containing over five-thousand first names, begin by sorting it into alphabetical order.<br>
Then working out the alphabetical value for each name,<br>
multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,<br>
is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?


```haskell
import Data.Char
import Names
```

I begin by importing `Names` and `Data.Char`. The module `Names` exports its only function: `names :: [String]`.<br> `Data.Char` is useful because `ord :: Char -> Int` gives a way to convert character to numbers.<br>
Next, I need a way to sort the list and so I implement a quick sort.


```haskell
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort.smalls) (x:xs) ++ [x] ++ (qsort.bigs) (x:xs)
  where
    smalls (a:as) = [t | t<-as, t < a]
    bigs   (a:as) = [t | t<-as, t > a]
```

`charToInt :: Char -> Integer` and `nameToInt :: String -> Integer` are defined next. The both return<br>
`Integer` type so that I needn't worry about block size. The first converts a given character to its place-value<br>
in the alphabet. The second provides a method for converting an entire word to the sum place-values of its characters.<br>


```haskell
charToInt :: Char -> Integer
charToInt alpha =  fromIntegral $ ord alpha - 64

nameToInt :: String -> Integer
nameToInt name = sum.map charToInt $ name
```

Lastly, `challenge22` computes the project euler challenge.


```haskell
challenge22 :: Integer
challenge22 = sum [i * nameToInt name | (i,name) <- zip [1..] (qsort names)]
```


```haskell
challenge22
```


    871198282

