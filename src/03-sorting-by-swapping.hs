
newtype Lf v = In   { insideI :: v (Lf v )}
newtype Gf x = OutO { insideO :: x (Gf x )}


data K = K
       deriving (Ord, Eq)


data KList l = KNil 
             | KCons  K  l
data SList l = SNil
             | SCons  K  l

fold      ::  (Functor f) => (f a ->   a)       -> Lf f -> a
unfold     :: (Functor x) => (  a -> x a) ->  a -> Gf x

unfold     f = OutO . fmap (unfold f)     . f
fold       f =                              f    . fmap (fold f)     . insideI


instance Functor KList where
     fmap f  KNil       = KNil
     fmap f (KCons k x) = KCons k (f x)


instance Functor SList where
     fmap f SNil = SNil
     fmap f (SCons k l) = SCons k (f l)

bubbleSort :: Lf KList -> Gf SList

bubbleSort = unfold bubble
     where          bubble = fold bub


-- |
--
-- bub's forward type shows you that both:
--
-- nIns      ::  KList (Gf SList ..........)  > SList (KList (Gf SList))
-- bub       ::  KList (.. SList (Lf ...  )) -> SList (..... (Lf .....))

nIns      ::  KList (Gf SList)         -> SList (KList (Gf SList))
bub       ::  KList (SList (Lf KList)) -> SList (Lf KList)


--However, the (difunctor-reversed) signature has the identical shape to bub's:

-- bubInv    ::  SList (Lf KList)         -> KList (SList (Lf KList))
-- nIns      ::  KList (Gf SList)         -> SList (KList (Gf SList))
-- 
-- -- And furthermore, when you do the following (dual) interchange:
-- --
-- --    Gf <-> Lf
-- -- KList <-> SList
-- -- 
-- -- Scripted out here as a series of regexes:
-- --
-- -- { . <-- bubInv, s/SList/Klist/
-- --               , s/SList/Klist/
-- --               , s/Gf/LF/
-- --               , s/Lf/GF/
-- --               , s/list/List/
-- --               , s/GF/Gf/
-- --               , s/LF/Lf/
-- -- }
-- nIns      ::  KList (Gf SList)         -> SList (KList (Gf SList))
-- bubInv    ::  KList (Gf SList)         -> SList (KList (Gf SList))

swap :: KList (SList x) -> SList (KList x)



bub (KNil                         )     = SNil
bub (KCons a        (SNil     )   )     = SCons a (In (KNil     ))
bub (KCons a        (SCons b x)   )
    | (<=) a               b            = SCons a (In (KCons b x))
    | otherwise                         = SCons b (In (KCons a x))
nIns (KNil                        )     = SNil
nIns (KCons a (OutO (SNil     ) ) )     = SCons a     (KNil     )
nIns (KCons a (OutO (SCons b x) ) )
     | (<=) a              b            = SCons a     (KCons b x)
     | otherwise                        = SCons b     (KCons a x)
swap KNil                               = SNil
swap (KCons a       (SNil     )   )     = SCons a (KNil)
swap (KCons a       (SCons b x)   )
     | (<=) a              b            = SCons a (KCons b x)
     | otherwise                        = SCons b (KCons a x)

