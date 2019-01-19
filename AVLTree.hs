-- An implementation of an AVL Tree in Haskell. A binary tree 
-- which is balanced in height after every operation and where
-- for each node with value x holds: 
--  * x is less than (<=) all elements in the right sub-tree (if exists), 
--  * x is larger than (>=) all elements in the left sub-tree (if exists.
--
-- Implementation is fast and easy. Not optimal but perfect for 
-- code-challenges and educational interest. It contains a nice tree
-- to string function as well:
--
-- 
--             ******* 2
--             *
--     ******* 3
--     *       *
--     *       ******* 5
--     *
--     7
--     *
--     *               ******* 9
--     *               *
--     *       ******* 10
--     *       *       *
--     *       *       ******* 11
--     *       *
--     ******* 12
--             *
--             ******* 14
--
--

module AVLTree (fromList, 
                toList, 
                size, 
                insert, 
                singleton, 
                member, 
                delete, 
                empty, 
                isEmpty, 
                findMin, 
                findMax, 
                deleteMin, 
                deleteMax) where


data AVLTree a = Empty | Node a Int (AVLTree a) (AVLTree a)


-- Insert an element in an AVL Tree.
-- time complexity: O(log n) 
insert :: Ord a => a -> AVLTree a -> AVLTree a
insert x Empty            = Node x 1 Empty Empty
insert x (Node t _ l r) 
    | t < x               = rebalance (Node t h1 l r')
    | otherwise           = rebalance (Node t h2 l' r)
    where
    r'         = insert x r
    h1         = 1 + max (height l) (height r')
    l'         = insert x l
    h2         = 1 + max (height l') (height r)


-- Delete an element from an AVL Tree. If the element is not there
-- the empty tree is returned.
-- time complexity: O(log n)
delete :: Ord a => a -> AVLTree a -> AVLTree a
delete _ Empty          = Empty
delete x (Node t _ l r) 
    | t < x             = rebalance (Node t h1 l r')
    | t == x            = merge l r 
    | otherwise         = rebalance (Node t h2 l' r)
    where
    r'         = delete x r 
    h1         = 1 + max (height l) (height r')
    l'         = delete x l
    h2         = 1 + max (height l') (height r)


-- Merge two trees A and B such that 
--   *)  findMax A <= findMin B 
--   *)  |height A - height B| <= 1
merge :: Ord a => AVLTree a -> AVLTree a -> AVLTree a
merge Empty v       = v
merge u Empty       = u
merge x y
    | height x < height y  = Node u h1 x y'
    | otherwise            = Node v h2 x' y
    where
    (u, y') = deleteMin y
    h1      = 1 + max (height x) (height y')
    (v, x') = deleteMax x  
    h2      = 1 + max (height x') (height y)


-- Returns the empty AVL Tree.
empty :: Ord a => AVLTree a
empty = Empty 


-- Checks if an AVL Tree is empty.
-- time complexity: O(1)
isEmpty :: Ord a => AVLTree a -> Bool
isEmpty Empty = True
isEmpty _     = False 


-- Construct an AVL Tree from a single element 
-- time complexity: O(1)
singleton :: Ord a => a -> AVLTree a
singleton x  = insert x Empty 


-- Constructs an AVL Tree form a list of elements
-- time complexity: O(n log n)
fromList :: Ord a => [a] -> AVLTree a
fromList ls = foldr insert Empty ls  


-- Turn an AVL Tree into a list.
-- time complexity: O(n)
toList :: Ord a => AVLTree a -> [a]
toList Empty            = []
toList (Node t _ l r)   = (toList l) ++ (t : toList r)


-- Rebalance an AVL Tree. 
-- time complexity: O(1)
rebalance :: Ord a => AVLTree a -> AVLTree a
rebalance (Node t h l r)
    | (height l) + 1 < height r     = rotateToLeft (Node t h l r)
    | height l > (height r) + 1     = rotateToRight (Node t h l r)
    | otherwise                     = Node t h l r


-- Rotations to support the rebalance function.
-- time complexity: O(1)
rotateToRight, rotateToLeft :: Ord a => AVLTree a -> AVLTree a
rotateToRight (Node t _ (Node t' _ l' r') r) =
    let
    h1     = 1 + max (height r') (height r)
    h2     = 1 + max (height l') h1     
    in
    Node t' h2 l' (Node t h1 r' r)
rotateToLeft  (Node t _ l (Node t' _ l' r')) =
    let
    h1     = 1 + max (height l) (height l')
    h2     = 1 + max h1 (height r')
    in
    Node t' h2 (Node t h1 l l') r'


-- Gives the height of an AVL Tree. Empty AVL Trees have height 0.
-- time complexity: O(1)
height :: Ord a => AVLTree a -> Int
height Empty            = 0
height (Node _ h _ _)   = h


-- Gives the size of an AVL Tree
-- time complexity: O(n)
size :: Ord a => AVLTree a -> Int
size Empty              = 0
size (Node _ _ l r)     = 1 + size l + size r


-- Gives the largest and the smallest element from a non-empty
-- AVL Tree. The non-empty condition is not checked.
-- time complexity: O(log n) 
findMin, findMax :: Ord a => AVLTree a -> a
findMin (Node t _ Empty _) = t
findMin (Node t _ l _)     = findMin l 
findMax (Node t _ _ Empty) = t
findMax (Node t _ _ r)     = findMax r 


-- Operations for deleting the largest and the smallest element from 
-- a given non-empty AVL Tree. The non-empty condition is nog checked. 
-- time complexity: O(log n) 
deleteMin, deleteMax :: Ord a => AVLTree a -> (a, AVLTree a)
deleteMin (Node t _ Empty r) = (t, r)
deleteMin (Node t _ l r)     = 
    let 
    (y, l') = deleteMin l
    h'      = 1 + max (height l') (height r)
    in
    (y, rebalance (Node t h' l' r))
deleteMax (Node t _ l Empty) = (t, l)
deleteMax (Node t _ l r)     = 
    let 
    (y, r') = deleteMax r 
    h'      = 1 + max (height l) (height r')
    in 
    (y, rebalance (Node t h' l r'))


-- Checks if an element is a member of an AVL Tree
-- time complexity: O(log n) 
member :: Ord a => a -> AVLTree a -> Bool
member x Empty          = False
member x (Node t _ l r)
    | x < t         = member x l
    | x == t        = True
    | otherwise     = member x r


-- Implementation of the show function, gives a string representation
-- of the AVL Tree. 
instance (Show a) => Show (AVLTree a) where
    show tr = (++) "\n" . unlines . snd . toLines $ tr

        where

        toLines :: (Show a) => AVLTree a -> (Int, [String])
        toLines Empty                   = (0, [""])
        toLines (Node t _ Empty Empty)  = (0, [" " ++ show t])
        toLines (Node t _ l r)          = (ln + 1, lv_new ++ [" *"] ++ [" " ++ show t] ++ [" *"] ++ rv_new)
            where
            (il, lv)    = toLines l
            (ir, rv)    = toLines r
            ln          = length lv
            rn          = length rv
            lv_sub      = (replicate il "        ") ++ [" *******"] ++ (replicate (ln - il) " *      ")
            rv_sub      = (replicate ir " *      ") ++ [" *******"] ++ (replicate (rn - ir) "        ")
            lv_new      = zipWith (++) lv_sub lv
            rv_new      = zipWith (++) rv_sub rv

