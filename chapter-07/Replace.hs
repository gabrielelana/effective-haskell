{-# LANGUAGE RecordWildCards #-}
module Replace where

import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

data Env = Env { path :: FilePath
               , needle :: String
               , replacement :: String
               }

fromEnvironment :: [String] -> Maybe Env
fromEnvironment [path, needle, replacement] = Just $ Env {..}
fromEnvironment _ = Nothing

runReplace :: Maybe Env -> IO String
runReplace Nothing = return "Not enough arguments"
runReplace (Just Env {..}) = unwords . (fmap $ replaceWord needle replacement) . words <$> readFile path
  where replaceWord :: String -> String -> String -> String
        replaceWord needle replacement word
          | needle == word = replacement
          | otherwise = word

main = (fromEnvironment <$> getArgs) >>= runReplace >>= putStrLn
