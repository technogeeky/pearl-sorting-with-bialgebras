{-# LANGUAGE TypeOperators #-}
module Pearl.SWB.Section05 where

import Pearl.SWB.Section03 (Gf(..), Lf(..), fold, unfold, KList(..), SList(..))


import Pearl.SWB.Section04 (FixO, FixI, apo, para, (/\), (\/), (:\/:)(..),  (:/\:)(..) )

insertSort :: Lf KList -> Gf SList
selectSort :: Lf KList -> Gf SList
insertSort' :: Lf KList -> Gf SList
selectSort' :: Lf KList -> Gf SList

insertSort  = fold   insert where  insert = apo   ins
insertSort' = fold   insert where  insert = apo   (swop . fmap (id /\ insideO)       )
selectSort  = unfold select where  select = para  sel
selectSort' = unfold select where  select = para  (fmap        (id \/ In     ) . swop)


-- insertSort which takes advantage that the list being inserted into, is already sorted:

nIns (KNil                         )    = SNil
nIns (KCons a (OutO  (SNil     ) ) )    = SCons a             (KNil     )
nIns (KCons a (OutO  (SCons b x) ) )
     |      a      <=       b           = SCons a             (KCons b x)
     |          otherwise               = SCons b             (KCons a x)

ins  KNil                               = SNil
ins  (KCons a (OutO  (SNil      ) ) )   = SCons a (Paws (OutO (SNil      ) ) )
ins  (KCons a (OutO  (SCons b x') ) )
     |      a      <=       b           = SCons a (Paws (OutO (SCons b x') ) )
     |          otherwise               = SCons b (Play       (KCons a x')   )
-- ins  (KCons a (OutO  (SNil      ) ) )   = SCons a (Play (     (KNil      ) ) )


-- ^
-- >>> :t ins

-- ^
-- >>> :t ins
-- ins :: KList (Gf SList) -> SList (l        :\/: KList l1)
-- ins :: KList (Gf SList) -> SList (Gf SList :\/: r)
-- ins :: KList (Gf SList) -> SList (Gf SList :\/: KList (Gf SList))


swop :: KList (l :/\: SList l1) -> SList (l :\/: KList l1)
swop  KNil                               = SNil
swop  (KCons a (As x (SNil      ) ) )    = SCons a (Paws (        x         ) )
swop  (KCons a (As x (SCons b x') ) )
      |      a      <=       b           = SCons a (Paws (        x         ) )
      |          otherwise               = SCons b (Play (KCons a x'        ) )

sel :: KList (Lf KList :/\: SList (Lf KList)) -> SList (Lf KList)

sel   KNil                               = SNil
sel   (KCons a (As x (SNil      ) ) )    = SCons a (     (        x         ) )
sel   (KCons a (As x (SCons b x') ) )
      |      a      <=       b           = SCons a (     (        x         ) )
      |          otherwise               = SCons b (In   (KCons a x'        ) )

bub   (KNil                         )    = SNil
bub   (KCons a        (SNil     )   )    = SCons a (In    (KNil     ))
bub   (KCons a        (SCons b x)   )
      | (<=) a               b           = SCons a (In    (KCons b x))
      | otherwise                        = SCons b (In    (KCons a x))

