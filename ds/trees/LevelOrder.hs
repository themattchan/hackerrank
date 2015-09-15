data Tree a = Leaf | Branch a (Tree a) (Tree a)

levelOrder :: Tree a -> [a]
levelOrder t = go [t]
  where
    go [] = []
    go (Leaf : xs) = go xs
    go (Branch a l r : xs) = a : go (xs ++ [l,r])

tree = Branch 1
            (Branch 2
                  (Branch 4
                        (Branch 7 Leaf Leaf)
                        Leaf)
                  (Branch 5 Leaf Leaf))
            (Branch 3
                  (Branch 6
                        (Branch 8 Leaf Leaf)
                        (Branch 9 Leaf Leaf))
                  Leaf)
