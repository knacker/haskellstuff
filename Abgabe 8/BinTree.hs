data BinTree el = Empty
                | Tree (BinTree el) el (BinTree el)
                deriving (Eq, Ord, Show)
                
makeEmptyBinTree :: BinTree el 
makeEmptyBinTree = Empty

makeBinTree :: el -> BinTree el 
makeBinTree el = Tree (Empty) el (Empty)

insert :: (Ord el) => BinTree el -> el -> BinTree el 
insert Empty el = makeBinTree el 
insert t@(Tree left a right) b | a == b = t
                               | a < b = Tree left a (insert right b)
                               | otherwise = Tree (insert left b) a right

--erstellt einen bintree, mit einem leeren bintree als input. Beispiel eingabe : createBinTree [3,4,5,7,8]                              
createBinTree :: (Ord el) => [el] -> BinTree el
createBinTree [] = Empty
createBinTree (x:xs) = insert (createBinTree xs) x --aufruf, wenn ersteller baum übergeben wird

nodesInOrder :: (Ord el, Eq el) => BinTree el -> [el]
nodesInOrder Empty = []
nodesInOrder t@(Tree left a right) = nodesInOrder left ++ [a] ++ nodesInOrder right

nodesPreOrder :: (Ord el, Eq el) => BinTree el -> [el]
nodesPreOrder Empty = []
nodesPreOrder t@(Tree left a right) = [a] ++ nodesPreOrder left ++ nodesPreOrder right

nodesPostOrder :: (Ord el, Eq el) => BinTree el -> [el]
nodesPostOrder Empty = []
nodesPostOrder t@(Tree left a right) = nodesPreOrder left ++ nodesPreOrder right ++ [a]

nodesLevelOrder :: (Ord el, Eq el) => BinTree el -> [el]
nodesLevelOrder tree = iterate [tree]
    where
        iterate [] = []
        iterate (Empty:xs) = iterate xs
        iterate (Tree left a right:xs) = a : iterate(xs ++ [left, right])
--geht solange den baum entlang, bis empty erreicht wurde
leaves :: (Ord el, Eq el) => BinTree el -> [el]
leaves Empty = []
leaves t@(Tree left a right) = if left == Empty && right == Empty then [a] ++ leaves left ++ leaves right
                                                                  else leaves left ++ leaves right
                                                                  
binTreeMap :: (t -> el) -> BinTree t -> BinTree el
binTreeMap f Empty = Empty
binTreeMap f (Tree Empty a Empty) = (Tree Empty (f a) Empty)                                                                
binTreeMap f (Tree left a right) = Tree (binTreeMap f left) (f a) (binTreeMap f right)

--veränderte fold funktion
binTreeFold :: (acc -> el -> acc -> acc) -> acc -> BinTree el -> acc
binTreeFold f acc Empty = acc
binTreeFold f acc (Tree left a right) = f (binTreeFold f acc left) a (binTreeFold f acc right)

--gleiches prinzip wie oben, nur mit lambda notation
nodesInOrderFold :: BinTree el -> [el]
nodesInOrderFold Empty = []
nodesInOrderFold t@(Tree left a right) = binTreeFold(\left a right -> left ++ [a] ++ right) [] t

nodesPreOrderFold :: BinTree el -> [el] 
nodesPreOrderFold Empty = [] 
nodesPreOrderFold t@(Tree left a right) = binTreeFold(\left a right -> [a] ++ left ++ right) [] t

nodesPostOrderFold :: BinTree el -> [el]
nodesPostOrderFold Empty = []
nodesPostOrderFold t@(Tree left a right) = binTreeFold(\left a right -> left ++ right ++ [a]) [] t

--wenn links und rechts leer sind, dann ist es ein blatt und wird der liste hinzugefügts
leavesFold :: (Ord el) =>BinTree el -> [el]
leavesFold Empty = []
leavesFold t@(Tree left a right) = binTreeFold(\left a right -> if left ++ right == [] then [a] else []) [] t