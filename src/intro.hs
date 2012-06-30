{-# LANGUAGE UnicodeSyntax #-}

import Data.List 
               ( unfoldr   {-  unfoldr   ::        (b → Maybe (a,b)) → b → [a]         -}
               , partition {-  partition ::        (a -> Bool) -> [a] -> ([a], [a])    -}
               , delete    {-  delete    :: Eq a => a -> [a] -> [a]                    -}
               )

insertSort :: [Integer] → [Integer]
insertSort = foldr insert [ ]

insert :: Integer → [Integer] → [Integer]
insert y ys = xs ++ [y] ++ zs
     where (xs,zs) = partition (<y) ys


selectSort :: [Integer] → [Integer]
selectSort = unfoldr select

select :: [Integer] → Maybe (Integer,[Integer])
select [ ] = Nothing
select xs= Just (x,xs0)
  where x   = minimum xs 
        xs0 = delete x xs

