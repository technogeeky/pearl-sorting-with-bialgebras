{-# LANGUAGE UnicodeSyntax #-}

module Pearl.SWB.Section01 where

import Data.List 
               ( unfoldr   {-  unfoldr   ::        (b → Maybe (a,b)) → b → [a]         -}
               , partition {-  partition ::        (a -> Bool) -> [a] -> ([a], [a])    -}
               , delete    {-  delete    :: Eq a => a -> [a] -> [a]                    -}
               )

-- Sorting algorithms are often the ﬁrst non-trivial programs that are
-- introduced to ﬂedgling programmers. The problem of sorting lends
-- itself to a myriad of algorithmic strategies with varying asymptotic
-- complexities to explore, making it an ideal pedagogical tool. Within
-- the  functional  programming  community,  the  insertion  sort  also
-- serves to exemplify the use of folds, where the key is to deﬁne an
-- appropriate function insert which inserts a value into a sorted list.


{- |||| insertSort :: [Integer] → [Integer] -}


-- The  insertion  function  partitions  a  given  list  into  two  using  an
-- ordering of its elements with respect to the value to be inserted.
-- This value is then inserted in between the partitions:

{- |||| insert :: Integer → [Integer] → [Integer] -}

-- This is an entirely routine and naïve deﬁnition, which makes use of
-- the partition function from the list utilities section of the Haskell
-- Report. When the input list ys is ordered, insert y ys adds y to the
-- list ys and maintains the invariant that the ensuing list is ordered.
-- Thus, we are able to fold an unordered list into an ordered one when
-- we start with an empty list as the initial value of the fold.


-- Perhaps less well known is that an alternative sorting algorithm,
-- selection sort, can be written in terms of an unfold. An unfold can
-- be thought of as the dual of a fold: a fold consumes a list, whereas
-- unfold produces a list, as evident in the type of unfoldr:


{- |||| unfoldr   ::        (b → Maybe (a,b)) → b → [a]         -}

-- A selection sort constructs an ordered list by repeatedly extracting
-- the least element from an unordered list. This effectively describes
-- an unfold where the input seed is an unordered list that is used to
-- produce an ordered list:


{- |||| selectSort :: [Integer] → [Integer] -}


-- The function select removes the least element from its input list,
-- and returns that element along with the original list with the ele-
-- ment removed. When the list is empty, the function signals that the
-- unfolding must ﬁnish.

{- |||| select :: [Integer] → Maybe ( Integer  , [Integer] ) -}

-- With a little intuition, one might see that these two sorting al-
-- gorithms are closely related, since they fundamentally complement
-- one another on two levels: folds dualise unfolds, and insertion du-
-- alises selection. However, the details of this relationship are some-
-- what shrouded by our language: the connection between the in-
-- gredients of insert and select is difﬁcult to spot since append and
-- partition seem to have little to do with minimum and delete. Fur-
-- thermore, the rendition of insert and select in terms of folds and
-- unfolds is not straightforward.

-- In order to illuminate the connection, we use a type-driven ap-
-- proach to synthesise these algorithms, where notions from category
-- theory are used to guide the development. As we shall see, naïve
-- variants of insert and select can be written as an unfold and fold,
-- respectively, thus revealing that they are in fact dual. As a conse-
-- quence, each one gives rise to the other in an entirely mechanical
-- fashion: we effectively obtain algorithms for free. We will obtain
-- the true select and insert with alternative recursion schemes.

-- Of  course,  both  of  these  algorithms  are  inefﬁcient,  taking
-- quadratic  time  in  the  length  of  the  input  list  to  compute,  and
-- in practice these toy examples are soon abandoned in favour of
-- more practical sorting algorithms. As it turns out, our venture into
-- understanding  the  structural  similarities  between  insertSort  and

-- selectSort will not be in vain: the insights we shall gain will be-
-- come useful when we investigate more efﬁcient sorting algorithms.

-- The main contributions of the paper are as follows:

-- •  A type-driven approach to the design of sorting algorithms us-
-- ing folds and unfolds, which we then extend to paramorphisms
-- and apomorphisms in order to improve efﬁciency.

-- •  An equivalence of sorting algorithms, which allows us to for-
-- malise folkloric relationships such as the one between insertion
-- and selection sort.

-- •  Algorithms for free; because the concepts we use to develop
-- these  algorithms  dualise,  each  sorting  algorithm  comes  with
-- another for free.

-- •  As a consequence of this formalisation, we relate bialgebras
-- and distributive laws to folds of apomorphisms and unfolds of
-- paramorphisms.



-- We continue this paper with a gentle introduction to folds, un-
-- folds, and type-directed algorithm design in Section 2. Then, we
-- delve into sorting by swapping in Section 3, deﬁning two sorting
-- algorithms at once using a distributive law with folds and unfolds.
-- In Section 4, we introduce para- and apomorphisms and use them
-- to deﬁne insertion and selection sort in Section 5. We move on
-- to faster sorting algorithms in Section 6 (quicksort) and Section 7
-- (heapsort). Finally, we review related work in Section 8, and con-
-- clude in Section 9.



insertSort :: [Integer] → [Integer]
selectSort :: [Integer] → [Integer]

insertSort = foldr   insert [ ]
selectSort = unfoldr select

insert ::  Integer →         [Integer] → [Integer]
select :: [Integer] → Maybe ( Integer  , [Integer] )

insert y ys = xs ++ [y] ++ zs
    where  {- || -}     {- || -}
             (xs     ,     zs) = partition (<y) ys

select []  = Nothing
select xs  = Just (x,xs0)
         where 
       x   = minimum xs 
       xs0 = delete x xs

