-- solution for http://blog.plover.com/math/17-puzzle.html

import Data.List (delete, permutations)

numbers = [6,6,5,2] -- ++ [2,3]
target = 17

data Tree o a = Branch o (Tree o a) (Tree o a) | Leaf a | Empty deriving (Show, Eq)

data Op = Add | Sub | Mult | Div deriving (Show, Eq)

op2f :: Fractional a => Op -> a -> a -> a
op2f Add  = (+)
op2f Sub  = (-)
op2f Mult = (*)
op2f Div  = (/)

pretty :: (Show a, Show op) => Tree op a -> String
pretty Empty = "?"
pretty (Leaf n) = show n
pretty (Branch op t1 t2) =  "(" ++ pretty t1 ++ ") "
                        ++ show op
                        ++ " (" ++ pretty t2 ++ ")"

solve :: Fractional a => Tree Op a -> a
solve Empty = 0
solve (Leaf x) = x
solve (Branch op t1 t2) = (op2f op) (solve t1) (solve t2)

result :: [Tree Op Double]
result = filter ((target ==) . solve)
       $ foldMap (foldr ((=<<) . add) [Empty]) -- `(=<<) . add` equivale a `\n ts -> ts >>= add n`
       $ permutations numbers

add :: Fractional a => a -> Tree Op a -> [Tree Op a]
add n Empty = [Leaf n]
add n (Leaf x) = Branch <$> [Add, Mult, Sub, Div] <*> [Leaf n] <*> [Leaf x]
add n (Branch op t1 t2) = (Branch op <$> [t1] <*> add n t2)
                       ++ (Branch op <$> add n t1 <*> [t2])

main :: IO ()
main = do
  putStrLn (show (length result) ++ " results")
  --mapM_ (print . pretty) result --all results
  print $ pretty $ head result
