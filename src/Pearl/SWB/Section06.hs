{-# LANGUAGE TypeOperators #-}

module Pearl.SWB.Section06 where


import Pearl.SWB.Section03 (
                   K(..)
                 , Gf(..)
                 , Lf(..)
                 , KList(..)
                 , SList(..)
                 , fold
                 , unfold
                 )
import Pearl.SWB.Section04 (
                   (:/\:)(..)
                 , (:\/:)(..)
                 , ( /\ )
                 , ( \/ )
                 , para
                 , apo
                 )

data KTree t = KE
            |  KY  t K t


type STree = KTree

instance Functor KTree where
     fmap f  KE        = KE
     fmap f (KY l k r) = KY (f l) k (f r)



(//\\) l k r = KY l k r

(//)   l k   = KY  l   k (id)
(\\)     k r = KY (id) k  r


pivot :: KList (STree (Lf KList)) -> STree (Lf KList)
pivot KNil                    = KE
pivot (KCons a  KE       )    = (//\\) (In (KNil     )) a (In (KNil     ))
pivot (KCons a (KY l b r))
      |      a    <=     b    = (//\\) (In (KCons a l)) b (      r      )
      |        otherwise      = (//\\) (      l       ) b (In (KCons a r))
-- ^
-- >>> :t pivot
-- pivot :: KList (KTree t) -> KTree (Lf KList)
-- pivot :: KList (KTree (Lf KList)) -> KTree (Lf KList)

-- GHC infers pivot to have KTree parts, but we want STree parts. So just change it!


sprout :: KList (l :/\: STree l) -> STree (l :\/: KList l)
sprout KNil                          = KE
sprout (KCons a (As t  KE         )) = (//\\) (Paws          t ) a  (Paws          t )
sprout (KCons a (As t (KY l b r ) ))
       |      a     <=      b        = (//\\) (Play (KCons a l)) b  (Paws          r )
       |         otherwise           = (//\\) (Paws l)           b  (Play (KCons a r))


-- ^
-- >>> :t sprout
-- sprout :: KList (l :/\: KTree l) -> KTree (l :\/: KList l)
--
-- Again, GHC infers the KTree type instead of the STree type

-- ^
-- >>> :t treeIn
treeIn :: KList (Gf STree) -> STree (Gf STree :\/: KList (Gf STree))
treeIn KNil                          = KE
treeIn (KCons a (OutO  KE         )) = (//\\) (Paws (OutO KE)  ) a  (Paws (OutO KE  ))
treeIn (KCons a (OutO (KY l b r ) ))
       |      a     <=      b        = (//\\) (Play (KCons a l)) b  (Paws          r )
       |         otherwise           = (//\\) (Paws l)           b  (Play (KCons a r))

-- ^
-- >>> :t treeIn
-- treeIn :: KList (Gf KTree) -> KTree (Gf KTree :\/: KList (Gf KTree))
--
-- GHC still wants KTrees instead of STrees..


grow  :: Lf KList -> Gf STree
grow' :: Lf KList -> Gf STree


grow  = unfold (para (         fmap (id \/ In     ) . sprout))
grow' = fold   (apo  (sprout . fmap (id /\ insideO)         ))



glue :: STree (Gf SList) -> SList (Gf SList :\/: STree (Gf SList))

glue KE                            = SNil
glue (KY (OutO (SNil     )) a r)   = SCons a (Paws             r  )
glue (KY (OutO (SCons b l)) a r)   = SCons b (Play ((//\\) l a r) )

-- ^
-- >>> :t glue
-- glue :: KTree (Gf SList) -> SList (Gf SList :\/: KTree (Gf SList))

ternaryAppend = apo glue

withr :: STree (t :/\: SList t) -> SList (t :\/: STree t)
withr KE                                     = SNil
withr (KY (As l (SNil      )) a (As r _ ))   = SCons a (Paws              r  )
withr (KY (As l (SCons b l')) a (As r _ ))   = SCons b (Play ((//\\) l' a r) )

-- ^
-- >>> :t withr
-- withr :: KTree (t :/\: SList t) -> SList (t :\/: KTree t)

shear :: STree (Lf STree :/\: SList (Lf STree)) -> SList (Lf STree)
shear KE                                     = SNil
shear (KY (As l (SNil      )) a (As r _ ))   = SCons a r
shear (KY (As l (SCons b l')) a (As r _ ))   = SCons b (In ( (//\\) l' a r ))

ex00 :: Lf KTree -> SList (Lf STree)
ex00 = para shear


flatten   :: Lf STree -> Gf SList
flatten'  :: Lf STree -> Gf SList

flatten  = fold   (apo  (withr . fmap (id /\ insideO)          ))
flatten' = unfold (para (        fmap (id \/ In     ) . withr  ))


westcast  :: (Functor x) => Lf x -> Gf x
eastcast  :: (Functor v) => Gf v -> Lf v
antifold   f =                              f    . fmap (antifold f) . insideO
antiunfold f = In   . fmap (antiunfold f) . f
westcast  = OutO         . fmap westcast . insideI
eastcast  = antifold (antiunfold (fmap  insideI))


downcast  :: (Functor v) => Gf v -> Lf v
upcast    :: (Functor x) => Lf x -> Gf x

downcast  = In           . fmap downcast . insideO


upcast    = fold     (unfold     (fmap  insideO)) 



quickSort = flatten . downcast . grow
treeSort  = flatten' . downcast . grow'
