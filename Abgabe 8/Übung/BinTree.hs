data BinTree el = Empty | Tree (BinTree el) el (BinTree el) deriving (Show, Ord, Eq)

makeEmptyBinTree :: BinTree el 
makeEmptyBinTree = Empty

makeBinTree :: el -> BinTree el 
makeBinTree el = Tree Empty el Empty

insert ::Ord el => el -> BinTree el -> BinTree el 
insert el Empty = Tree Empty el Empty
insert el t@(Tree left a right) = if el < a then Tree(insert el left) a (right) 
                                  else if el > a then Tree(left) a (insert el right)
                                  else t
                                  
createBinTree :: (Ord el) => [el]  -> BinTree el 
createBinTree [x] = Tree Empty x Empty
createBinTree (x:xs) = insert x (createBinTree xs)   

nodesInOrder :: (Ord el) => BinTree el -> [el]
nodesInOrder Empty = []
nodesInOrder (Tree Empty a Empty) = [a]
nodesInOrder t@(Tree left a right) = (nodesInOrder left) ++ [a] ++ (nodesInOrder right)

nodesInPreOrder :: (Ord el) => BinTree el -> [el]
nodesInPreOrder Empty = []
nodesInPreOrder (Tree Empty a Empty) = [a]
nodesInPreOrder (Tree left a right) = [a] ++ (nodesInPreOrder left) ++ (nodesInPreOrder right)

nodesPostOrder :: BinTree el -> [el]
nodesPostOrder Empty = []
nodesPostOrder (Tree Empty a Empty) = [a]
nodesPostOrder (Tree left a right) = (nodesPostOrder left) ++ (nodesPostOrder right) ++ [a]

leaves :: BinTree el -> [el]
leaves Empty = []
leaves (Tree Empty a right) = [a]
leaves (Tree left a right) = leaves left ++ leaves right

binTreeMap :: (a -> b) -> BinTree a -> BinTree b 
binTreeMap f (Tree Empty el Empty) = (Tree Empty (f el) Empty)
binTreeMap f (Tree left el right) = Tree (binTreeMap f left) (f el) (binTreeMap f right)

binTreeFold :: (acc -> el -> acc -> acc) -> acc -> BinTree el -> acc
binTreeFold f acc Empty = acc
binTreeFold f acc (Tree left a right) = f (binTreeFold f acc left) a (binTreeFold f acc right)

nodesInOrderFold :: BinTree el -> [el]
nodesInOrderFold Empty = []
nodesInOrderFold t@(Tree left a right) = binTreeFold(\left a right -> left ++ [a] ++ right) [] t