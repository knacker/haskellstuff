sortiert :: Ord a => [a] -> Bool
sortiert list = [head list <= head(tail list) | True] == [True]

sortiert2 :: Ord a => [a] -> Bool
sortiert2 (b:bs) = [head [x] <= head [y] | x <- b:bs, y <- bs] == [True]