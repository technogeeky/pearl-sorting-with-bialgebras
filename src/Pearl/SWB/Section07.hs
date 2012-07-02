{-# LANGUAGE TypeOperators #-}

module Pearl.SWB.Section07 where


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

import Pearl.SWB.Section06 (
                   KTree(..)
                 , STree(..)
                 , (//\\)
                 , downcast
                 )

type Heap = KTree



-- We start with the natural transformation from the KTree, sprout:
--
-- @
-- sprout :: KList (l :/\: STree l) -> STree (l :\/: KList l)
-- sprout KNil                          = KE
-- sprout (KCons a (As t  KE         )) = (//\\) (Paws          t ) a  (Paws          t )
-- sprout (KCons a (As t (KY l b r ) ))
--        |      a     <=      b        = (//\\) (Play (KCons a l)) b  (Paws          r )
--        |         otherwise           = (//\\) (Paws l)           b  (Play (KCons a r))
-- @
--

pile :: KList (l :/\: STree l) -> STree (l :\/: KList l)

pile    KNil                         = KE
pile   (KCons a (As t  KE         )) = (//\\) (Paws          t ) a  (Paws          t )
pile   (KCons a (As t (KY l b r ) ))
--   = (//\\) (Play (KCons  (a `max` b) l)) (a `min` b)   (Paws r)
--   = (//\\) (Play (KCons  (a `max` b) r)) (a `min` b)   (Paws l)
--   = (//\\) (Paws l)                      (a `max` b)   (Play (KCons (a `min` b) r))
     = (//\\) (Paws r)                      (a `max` b)   (Play (KCons (a `min` b) l)) 


-- ^
-- >>> :t pile


-- we want a:
--
--   KList    algebra
--    Heap co-algebra
--
--
-- So we start with treeIn:
--
treeIn :: KList (Gf STree) -> STree (Gf STree :\/: KList (Gf STree))
treeIn KNil                          = KE
treeIn (KCons a (OutO  KE         )) = (//\\) (Paws (OutO KE)  ) a  (Paws (OutO KE  ))
treeIn (KCons a (OutO (KY l b r ) ))
       |      a     <=      b        = (//\\) (Play (KCons a l)) b  (Paws          r )
       |         otherwise           = (//\\) (Paws l)           b  (Play (KCons a r))
-- 
-- ^
-- All we need to do is change the guards:
-- 
-- @
--     |      a     <=      b        = (//\\) (Play (KCons a l)) b  (Paws          r )
--     |         otherwise           = (//\\) (Paws l)           b  (Play (KCons a r))
-- becomes:
--     |      a     <=      b        = (//\\) (Play (KCons a l)) a  (Paws          l )
--     |         otherwise           = (//\\) (Play (KCons a r)) b  (Paws          l )
-- @


heapIn :: KList (Gf Heap ) -> Heap  (Gf Heap  :\/: KList (Gf Heap ))
heapIn KNil                          = KE
heapIn (KCons a (OutO  KE         )) = (//\\) (Paws (OutO KE)  ) a  (Paws (OutO KE  ))
heapIn (KCons a (OutO (KY l b r ) ))
       |      a     <=      b        = (//\\) (Play (KCons b l)) a  (Paws          l )
       |         otherwise           = (//\\) (Play (KCons a r)) b  (Paws          l )


blah0 = apo heapIn

-- Finally, we start with 'pivot' and end up with 'diivy':
--
--
pivot :: KList (STree (Lf KList)) -> STree (Lf KList)
pivot KNil                    = KE
pivot (KCons a  KE       )    = (//\\) (In (KNil     )) a (In (KNil     ))
pivot (KCons a (KY l b r))
      |      a    <=     b    = (//\\) (In (KCons a l)) b (      r      )
      |        otherwise      = (//\\) (      l       ) b (In (KCons a r))



-- Again, the top two cases are identical.
--
divvy :: KList (Heap  (Lf KList)) -> Heap  (Lf KList)
divvy KNil                    = KE
divvy (KCons a  KE       )    = (//\\) (In (KNil     )) a (In (KNil     ))
divvy (KCons a (KY l b r))
      |      a    <=     b    = (//\\) (In (KCons b r)) a (      l      )
      |        otherwise      = (//\\) (In (KCons a r)) b (      l      ) 
-- ^
-- as usual, we had to correct the guarded cases.

foldWithDivvy = fold divvy
unfoldWithDivvy = unfold (para ( fmap (id \/ In) . blend ))


sift :: Heap  (t :/\: SList t) -> SList (t :\/: Heap  t)
sift KE                                                     = SNil
sift (KY (As l  SNil        ) a  (As r ____________ ) )     = SCons a (Paws r)
sift (KY (As l  __________  ) a  (As r  SNil        ) )     = SCons a (Paws l)
sift (KY (As l (SCons b l') ) a  (As r (SCons d r') ) )
     |                b        <=             d             = SCons a (Play (KY l' b  r ))
     |                    otherwise                         = SCons a (Play (KY l  d  r'))


meld :: Heap  (Lf Heap  :/\: SList (Lf Heap )) -> SList (Lf Heap )
meld KE                                                     = SNil
meld (KY (As l  SNil        ) a  (As r ____________ ) )     = SCons a       r 
meld (KY (As l  __________  ) a  (As r  SNil        ) )     = SCons a       l 
meld (KY (As l (SCons b l') ) a  (As r (SCons d r') ) )
     |                b        <=             d             = SCons a (In (KY l' b  r ))
     |                    otherwise                         = SCons a (In (KY l  d  r'))

-- ^  meld is sometimes known as 'deleteMin' in a priorityqueue library

blend :: Heap  (t :/\: SList t) -> SList (t :\/: Heap  t)
blend KE                                                     = SNil
blend (KY (As l  SNil        ) a  (As r ____________ ) )     = SCons a (Paws r)
blend (KY (As l  __________  ) a  (As r  SNil        ) )     = SCons a (Paws l)
blend (KY (As l (SCons b l') ) a  (As r (SCons d r') ) )
      |                b        <=             d             = SCons a (Play (KY l' b  r ))
      |                    otherwise                         = SCons a (Play (KY l  d  r'))


blah = para meld

tMerge = apo (blend . fmap (id /\ insideO))


heapSort = unfold deleteMin . downcast . fold heapInsert
        where     deleteMin  = para meld
                  heapInsert = apo heapIn


mingleSort = fold tMerge . downcast . unfold (fold divvy)



data Bush b = Leaf K | Y b b
data List1 l1 = Single K | Push K l1
data Rose r = Rose K [r]
