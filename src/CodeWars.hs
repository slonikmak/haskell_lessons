module CodeWars where
import Data.Char



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

{-tribonacci _ n | n < 1 = []
tribonacci (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)-}

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci _ n | n < 1 = []
tribonacci (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)

titleCase :: String -> String -> String
titleCase minor title = let lowMinor = words $ map toLower minor
                            lowTitle = words $ map toLower title in undefined