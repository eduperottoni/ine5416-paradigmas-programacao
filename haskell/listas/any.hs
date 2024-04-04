any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (h:t) = f h || any' f t

-- any'' :: (a -> Bool) -> [a] -> Bool
-- any'' _ [] = False
-- any'' f(h:t) = if f h then True else f t

any''' f = foldr ((||) . f) False

main = do
    print (any''' even [2, 5, 9])

