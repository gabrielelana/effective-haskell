{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HCat (runHCat) where

import qualified System.Environment as Env
import Control.Exception (throwIO)
import GHC.IO.Exception (IOException(IOError))
import System.IO.Error (userError)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isSpace)
import System.Process (readProcess)
import System.IO (hGetChar, stdin, hSetEcho, openFile, IOMode(ReadMode))
import Data.Functor ((<&>))
import GHC.IO.Handle ( hSetBuffering, BufferMode(..) )
import qualified Data.ByteString as BS
import Control.Exception.Base (handle)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

data Action = NextPage
            | Stay
            | Quit
            deriving (Eq, Show)

showPages :: [T.Text] -> IO ()
showPages [] = return ()
showPages all@(page:pages) = do
  action <- handleInput
  case action of
    NextPage -> do
      clearScreen
      TIO.putStrLn page
      showPages pages
    Stay -> showPages all
    Quit -> return ()

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

handleInput :: IO Action
handleInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  getChar <&> \case ' ' -> NextPage
                    'q' -> Quit
                    _ -> Stay

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where parseArgs [] = Left "file name needed, none given"
        parseArgs [x] = Right x
        parseArgs _ = Left "multiple file names not supporded"

eitherToError :: Show a => Either a b -> IO b
eitherToError (Right b) = return b
eitherToError (Left a) = throwIO . userError $ show a

terminalSize :: IO ScreenDimensions
terminalSize = do
  screenRows <- read <$> readProcess "tput" ["lines"] ""
  screenColumns <- read <$> readProcess "tput" ["cols"] ""
  return ScreenDimensions{..}

paginate :: ScreenDimensions -> T.Text -> [T.Text]
paginate (ScreenDimensions {..}) t = T.unlines <$> pages screenColumns screenRows t

pages :: Int -> Int -> T.Text -> [[T.Text]]
pages nc nl t
  | t == T.empty = []
  | otherwise = let (page, rest) = nextPage nc nl t
                in page : pages nc nl rest

nextPage :: Int -> Int -> T.Text -> ([T.Text], T.Text)
nextPage nc nl t
  | nl == 0 || t == T.empty = ([], t)
  | otherwise = let (line, rest) = nextLine nc t
                    (lines, rest') = nextPage nc (nl - 1) rest
                in (line:lines, rest')

nextLine :: Int -> T.Text -> (T.Text, T.Text)
nextLine n t = go (n, T.length t, 0, 0) t
  where go (lineLimit, textLength, lastSpaceIndex, currentIndex) t =
          case T.index t currentIndex of
            _ | currentIndex >= lineLimit && lastSpaceIndex > 0 -> T.splitAt lastSpaceIndex t
            _ | currentIndex >= lineLimit || currentIndex >= textLength -> T.splitAt currentIndex t
            '\n' -> let (l, r) = T.splitAt currentIndex t in (l, T.drop 1 r)
            ' ' -> go (lineLimit, textLength, currentIndex, currentIndex + 1) t
            _ -> go (lineLimit, textLength, lastSpaceIndex, currentIndex + 1) t

runHCat :: IO ()
runHCat = handle handleError do
  filename <- handleArgs >>= eitherToError
  filehandle <- openFile filename ReadMode
  content <- TIO.hGetContents filehandle
  sd <- terminalSize
  showPages $ paginate sd content
  where handleError :: IOError -> IO ()
        handleError e = putStrLn "Error:" >> print e
