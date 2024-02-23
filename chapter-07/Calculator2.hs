{-# LANGUAGE TypeApplications #-}
module Calculator2 where

applyOp :: (Int -> Int -> Int) -> [Int] -> Int
applyOp op (x:xs) = foldl op x xs
applyOp op [] = 0

runLine :: String -> Int
runLine s = let h:t = words s in applyOp (op h) (read @Int <$> t)
  where op "+" = (+)
        op "*" = (*)
        op "-" = (-)
        op s = error $ "Not an operator: " <> s

main = runLine <$> getLine >>= (putStrLn . show)
