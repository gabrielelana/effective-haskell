{-# LANGUAGE TypeApplications #-}
module Calculator1 where

sumLine :: String -> Int
sumLine = sum . (fmap $ read @Int) . words

main = sumLine <$> getLine >>= (putStrLn . show)
