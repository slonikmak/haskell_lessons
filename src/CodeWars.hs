module CodeWars where


-- solution "abc" `shouldBe` ["ab", "c_"]
solution :: String->[String]
solution str = let parseStr [] acc = acc
                   parseStr [x] acc = acc ++ [x:'_':[]]
                   parseStr (x:y:xs) acc = parseStr xs acc ++ [x:y:[]] ++ acc in 
                   reverse $ parseStr str []

solution' :: String -> [String]
solution' [] = []
solution' (x:[]) = [[x,'_']]
solution' (x:y:xs) = [x,y]:(solution xs)

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, _, _) 1 = [a]
tribonacci (a, b, _) 2 = [a, b]
tribonacci (a, b, c) 3 = [a, b, c]

-- tribonacci' :: Num a => Int->[a]
-- tribonacci' 