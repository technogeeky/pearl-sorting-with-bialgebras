insertSort :: [Integer] → [Integer]
insertSort = foldr insert [ ]

insert :: Integer → [Integer] → [Integer]
insert y ys = xs ++ [y] ++ zs
where (xs,zs) = partition (<y) ys

unfoldr :: (b → Maybe (a,b)) → b → [a]

selectSort :: [Integer] → [Integer]
selectSort = unfoldr select

select :: [Integer] → Maybe (Integer,[Integer])
select [ ] = Nothing
select xs = Just (x,xs0)
 where x   = minimum xs xs0 = delete x xs

