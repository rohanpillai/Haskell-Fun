wordhelper:: String -> (String, String)
wordhelper "" = ("","")
wordhelper (x:xs) 
   | x == ' ' = ("", xs)
   | otherwise = (x:ys, zs)
  where (ys, zs) = wordhelper xs

words' :: String -> [String]
words' "" = []
words' as@(x:xs)
   | x == ' ' = words' xs
   | otherwise = ys: words' zs
  where (ys, zs) = wordhelper as

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ " " ++ unwords' xs
