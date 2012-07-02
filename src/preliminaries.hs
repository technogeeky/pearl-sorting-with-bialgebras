{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoMonomorphismRestriction #-}



newtype Lf v = In   { insideI :: v (Lf v )}
newtype Gf x = OutO { insideO :: x (Gf x )}

newtype Blah f g   = BlahO { outP :: f (Blah f g, Blah g f) }
newtype BBB  f g h = BBBO { outBBB :: f (BBB f g h, BBB g h f, BBB h f g) }


-- pointfree:
            {- In  =>       in       -}
            {- In a =>   |^| a |^|   -}
-- pointfree:
            {- OutO   =>    insideO   -}
            {- OutO a => |!| a |!| -}


outO = unfold (fmap insideO)
-- ^
-- >>> :t unfold (fmap insideO)
-- unfold (fmap insideO) :: Functor x => x (Gf x) -> Gf x

inO = fold (fmap In)
-- ^
-- >>> :t fold (fmap In)
-- fold (fmap In) :: Functor f => Lf f -> f (Lf f)



data K = K
--newtype K a i = K {unK :: a}


data List l = Nil 
            | Cons  K  l
                 {- ^ -- an ordered key type -}

instance Functor List where
     fmap f Nil = Nil
     fmap f (Cons k x) = Cons k (f x)

fold      ::  (Functor f) => (f a ->   a)       -> Lf f -> a
antifold  ::  (Functor f) => (f b ->   b)       -> Gf f -> b
unfold     :: (Functor x) => (  a -> x a) ->  a -> Gf x
antiunfold :: (Functor v) => (  b -> v b) ->  b -> Lf v

fold       f =                              f    . fmap (fold f)     . insideI
antifold   f =                              f    . fmap (antifold f) . insideO
unfold     f = OutO . fmap (unfold f)     . f
antiunfold f = In   . fmap (antiunfold f) . f

-- blah = foldr (fmap insideI)


downcast :: (Functor v) => Gf v -> Lf v
upcast   :: (Functor f) => Lf f -> Gf f

downcast  = In           . fmap downcast . insideO
upcast    = fold (unfold ( fmap            insideO )) 

-- > Then I stumbled over a blog entry of Shin-Cheng Mu [2] and from there
-- > over an article of Wadler [3], where the least fixpoint is encoded as
-- >
-- > Lfix X. F X  =  All X. (F X -> X) -> X.
-- >
-- > and the greatest fixpoint as
-- >
-- > Gfix X. F X  =  Exists X. (X -> F X) * X.


-- in  :                   F (Mu F) -> Mu F
-- out : Nu F        ->    F (Nu F)


-- cata_f . in = f . map cata_f
--                                             Mu f ~ forall x. (F x -> x) -> x
--                                             || |
-- cata :: forall x. (F x -> x)             -> Mu F -> x    
--  ana :: forall x. (       x -> F x) -> x -> Nu F
--                                             || |
--                                             Mu f ~ exists x. ((x -> F x) , x)

-- |
--                           <Lf, in          >
-- for any other F-algebra   (X , f : F X -> X), there's a unique algebra homomorphism:
--cata_f : Mu F -> X
--anam_f :         X -> Nu F
-- ^
-- for any other F-coalgebra (X , f :         X -> F X), a unique homomorphism:
--                           <Gf, out                  >



main = undefined











-- ^
-- >>> :t fold (unfold (fmap insideO))
-- fold (unfold (fmap insideO)) :: Functor x => Lf x -> Gf x

-- ^
-- >>> :t fold (\_ -> OutO { insideO = undefined })
-- fold (\_ -> OutO { insideO = undefined })
--   :: Functor f => Lf f -> Gf x

-- ^
-- >>> :t (\c -> fold (unfold c))
-- (\c -> fold (unfold c))
--   :: (Functor x, Functor f) =>
--      (f (Gf x) -> x (f (Gf x))) -> Lf f -> Gf x

-- ^
-- >>> :t (\a -> unfold (fold a))
-- (\a -> unfold (fold a))
--   :: (Functor f, Functor x) =>
--      (f (x (Lf f)) -> x (Lf f)) -> Lf f -> Gf x


-- ^
-- >>> :t In . fmap (id) . insideI
-- In . fmap (id) . insideI :: Functor v => Lf v -> Lf v

-- ^
-- >>> :t (\f -> fmap (unfold f) . f)
-- (\f -> fmap (unfold f) . f)
--   :: Functor f => (a -> f a) -> a -> f (Gf f)

-- ^
-- >>> :t (\f -> f. fmap (fold f))
-- (\f -> f. fmap (fold f))
--   :: Functor f => (f b -> b) -> f (Lf f) -> b


