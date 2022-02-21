\documentclass[12pt]{article}
%include polycode.fmt

\begin{document}
\begin{code}
module MonoidalMap where
{-# Language TypeSynonymInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
import Data.Monoid
import Data.Semigroup
import Data.Foldable

-- fmap2 = fmap . fmap 
sortByPair f a b = if (f b > f a) then (a,b) else (b,a)
\end{code}

We want to be able to implement decently efficient sparse vectors. We will represent sparse vectors as Maps that are Monoids (over addition and union) that use monoid operations to resolve conflicting $(k,v)$ pairs and Functors (so that we can implement scalar multiplication). As well, it'll be nice to have our Maps be multiplicative Semigroups under intersection and multiplication as well. 

\begin{code}
union :: (Ord k, Semigroup a) => Map k a -> Map k a -> Map k a
-- int :: (Ord k, Semigroup a) => Map k a -> Map k a -> Map k a
empty :: Map k a 
singleton :: k -> a -> Map k a

get :: Ord k => Map k a -> k -> a 
 
fromAssocs :: (Ord k, Semigroup a) => [(k,a)] -> Map k a
fromAssocs = mconcat . fmap (uncurry singleton) 

toAssocs :: Map k a -> [(k,a)]
toAssocs = treeFoldR (((:).) . curry id) []  

mMapMap :: (a -> b) -> Map k a -> Map k b 
treeFoldR :: (k -> a -> b -> b) -> b -> Map k a -> b



instance (Ord k, Semigroup a) => Monoid (Map k a) where
  mempty = empty
instance (Ord k, Semigroup a) => Semigroup (Map k a ) where
  (<>) = union

instance Functor (Map k) where
  fmap = mMapMap 
instance Foldable (Map k) where
  foldr = treeFoldR . const

data ProdMap k a = ProdMap {getProd :: Map k a}

-- instance (Ord k, Semigroup a) => Semigroup (ProdMap k a) where
--   ProdMap a <> ProdMap b = ProdMap $ a `int` b

-- newtype Vector k a = Vector {getVector :: Map k (Sum a)} deriving (Semigroup, Monoid)
-- type ProdVector k a = ProdMap k (Product a) 

-- toProdVector :: Vector k a -> ProdVector k a
-- toProdVector = ProdMap . fmap (Product . getSum) . getVector  

-- fromProdVector :: ProdVector k a -> Vector k a
-- fromProdVector = Vector . fmap (Sum . getProduct) . getProd

-- singletonV :: (k,a) -> Vector k a 
-- singletonV = Vector . singleton . fmap Sum

-- fromAssocsV :: (Ord k, Semigroup a) => [(k,a)] -> Vector k a 
-- fromAssocsV = Vector . fmap Sum . fromAssocs

-- toAssocsV :: Vector k a -> [(k,a)]
-- toAssocsV = toAssocs . fmap getSum . getVector 

-- instance Functor (Vector k) where
--   fmap f = Vector . fmap2 f . getVector
\end{code}

We will implement our map as a red black tree. A red-black tree can either be empty, or be a node with a color, a value, and two descendents. We now can also implement the empty and singleton constructors for |Map|, along with our functor method, |map|:

\begin{code}

data Map k a = Leaf | Node {color :: Color, value :: (k,a), lTree :: Map k a, rTree :: Map k a} deriving Show

data Color = Red | Black deriving (Show, Eq)

empty = Leaf
singleton k a = Node Black (k,a) Leaf Leaf
mMapMap _ Leaf = Leaf
mMapMap f (Node c (k,v) t1 t2) = Node c (k, f v) (mMapMap f t1) (mMapMap f t2) 
treeFoldR f = flip $ treeFoldR' f where 
  treeFoldR' f Leaf = id
  treeFoldR' f (Node _ v t1 t2) = treeFoldR' f t1 . uncurry f v . treeFoldR' f t2 
get (Node _ (k,a) t1 t2) k' 
  | k == k' = a
  | k > k' = get t1 k'
  | k < k' = get t2 k' 
\end{code}

Our standard |union| implementation relies on two helper operations, split and join. Our version of split will take a $(k,v)$ pair and a tree, split that tree into left and right subtrees according to the $(k,v)$ pair, and use $v$'s monoid operation to merge if a node with matching key is found in the tree. Our join operation will take a $(k,v)$ pair and trees entirely to the left and right of that pair respectively and merge them into a new tree. 

\begin{code}
split :: (Ord k, Semigroup a) => (k,a) -> Map k a -> (Map k a , (k,a), Map k a)
join :: Ord k => Map k a -> (k,a) -> Map k a -> Map k a

size :: Map k a -> Word
size Leaf = 0
size (Node _ _ t1 t2) = 1 + size t1 + size t2

union t1 t2 = 
  let (smallTree, bigTree) = sortByPair size t1 t2 in
    case bigTree of
      Leaf -> Leaf
      Node _ v bigLTree bigRTree -> 
        let (smallLTree, v', smallRTree) = split v smallTree in 
          join (union bigLTree smallLTree) v' (union bigRTree smallRTree) 
\end{code}
  
Splitting is recursive. We search down our tree looking for our key, checking our key against each node we come across. We have two possible final states. The first is that we reach a leaf: 
\begin{code}
split a Leaf = (Leaf, a, Leaf)
\end{code}
We can also terminate once we find a node with the same key:
\begin{code}
split (k,v) (Node _ (k',v') t1 t2) 
  | k == k' = (t1, (k, v<>v'), t2)
\end{code}
Otherwise, if our key is less than the value of the key in our node, we split our nodes left child. Our left subtree is then the left subtree of the child, while our right subtree is calculated by joining the right subtree of the child to the right child of the node. The greater than case is symmetric: 
\begin{code}
  | k < k' = 
    let (lLTree, a, rLTree) = split (k,v) t1 in 
      (lLTree, a, join rLTree (k',v') t2)
  | k > k' =
    let (lRTree, a, rRTree) = split (k,v) t2 in 
      (join t1 (k',v') lRTree, a, rRTree)
\end{code}

Joining two trees of equal black depth is easy:
\begin{code}
join t1 v t2 
  | rDepth t1 == lDepth t2 = rootRebalance (Node Red v t1 t2)
\end{code}
If the right tree is smaller, then we preemptively its root black in order to make rebalancing easier, and recursively search down the right branch of the left tree until we find a subtree of the same depth. We rebalance to preserve red-red invariance as we go along. The other case is symmetric. 
\begin{code}
  | rDepth t1 > lDepth t2 = rootRebalance $ rJoin v t1 $ colorBlack t2
  | rDepth t1 < lDepth t2 = rootRebalance $ lJoin v t2 $ colorBlack t1
  where
    rJoin v a b = 
      if rDepth a == lDepth b 
      then 
        Node Red v a b
      else
        case a of 
          Node Red nv t1 t2 -> Node Red nv t1 (rJoin v t2 b)
          Node Black nv t1 t2 -> rBalance nv t1 (rJoin v t2 b)
      where 
        rBalance vb t1 (Node Red vr1 (Node Red vr2 t2 t3) t4) = Node Red vr2 (Node Black vb t1 t2) (Node Black vr1 t3 t4) 
        rBalance vb t1 ( Node Red vr1 t2 (Node Red vr2 t3 t4)) = Node Red vr1 (Node Black vb t1 t2) (Node Black vr2 t3 t4)
        rBalance vb t1 t2 = Node Black vb t1 t2
    lJoin v a b = 
      if lDepth a == rDepth b 
      then
        Node Red v b a 
      else 
        case a of 
          Node Red nv t1 t2 -> Node Red nv (lJoin v t1 b) t2
          Node Black nv t1 t2 -> lBalance nv (lJoin v t1 b) t2 
      where 
        lBalance vb (Node Red vr1 t1 (Node Red vr2 t2 t3)) t4 = Node Red vr2 (Node Black vr1 t1 t2) (Node Black vb t3 t4)  
        lBalance vb (Node Red vr1 (Node Red vr2 t1 t2) t3) t4 = Node Red vr1 (Node Black vr2 t1 t2) (Node Black vb t3 t4)
        lBalance vb t1 t2 = Node Black vb t1 t2
    rootRebalance t = if hasRedChildren t then colorBlack t else t where
      hasRedChildren Leaf = False
      hasRedChildren (Node _ _ (Node Black _ _ _ ) (Node Black _ _ _)) = False
      hasRedChildren _ = True
    colorBlack Leaf = Leaf
    colorBlack (Node _ v t1 t2) = Node Black v t1 t2
    rDepth Leaf = 0
    rDepth (Node _ _ _ t) = 1 + rDepth t
    lDepth Leaf = 0
    lDepth (Node _ _ t _) = 1 + lDepth t
\end{code}  


\end{document}
