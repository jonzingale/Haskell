-- ABSTRACT DATA TYPE
module Stack (Stack, empty, isEmpty, push, top, pop) where
 
stack = f empty
  where
    f = foldr (.) id $  map (push) [2, 3, 4]

empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 
newtype Stack a = StackImpl [a] -- opaque!
empty = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)


-- implementation:
to_list :: Stack a -> [a]
to_list as | isEmpty as = []
           | otherwise = top as : (to_list.snd.pop) as