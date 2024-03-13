{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HCat4 (runHCat) where

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
  } deriving (Eq, Show)

data Action = NextPage
            | PrevPage
            | Reset
            | Stay
            | Quit
            deriving (Eq, Show)

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

showHelp :: T.Text -> IO ()
showHelp help = do
  BS.putStr "\^[[7m"
  TIO.putStrLn help
  BS.putStr "\^[[27m"

showPages :: ScreenDimensions -> [T.Text] -> [T.Text] -> IO ()
showPages _ _ [] = return ()
showPages sd@ScreenDimensions{..} contents originalPages@(page:pages) = go page [] pages
  where go page prevPages nextPages = do
          showPage page
          action <- handleInput
          case (action, prevPages, nextPages) of
            (NextPage, _, next:nextPages) -> go next (page:prevPages) nextPages
            (PrevPage, next:prevPages, _) -> go next prevPages (page:nextPages)
            (Reset, _, _) -> do
              sd' <- terminalSizeWithHelpLine
              let resizedPages = if sd /= sd' then paginate sd' contents else originalPages
                in showPages sd' contents resizedPages
            (Quit, _, _) -> clearScreen
            (_, _, _) -> go page prevPages nextPages
        showPage page = do
          clearScreen
          showHelp $ T.justifyLeft screenColumns ' ' "[n]: next, [p]: previous, [r]: reset, [q]: quit"
          TIO.putStr page

handleInput :: IO Action
handleInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  getChar <&> \case ' ' -> NextPage
                    'p' -> PrevPage
                    'n' -> NextPage
                    'r' -> Reset
                    'q' -> Quit
                    _ -> Stay

handleArgs :: IO (Either String [FilePath])
handleArgs = parseArgs <$> Env.getArgs
  where parseArgs [] = Left "file name needed, none given"
        parseArgs x = Right x

eitherToError :: Show a => Either a b -> IO b
eitherToError (Right b) = return b
eitherToError (Left a) = throwIO . userError $ show a

-- 🤢🤢🤢
-- better way to do this
considerHelpLine :: ScreenDimensions -> ScreenDimensions
considerHelpLine sd@ScreenDimensions{..} = sd { screenRows = screenRows - 2 }

terminalSize :: IO ScreenDimensions
terminalSize = do
  screenRows <- read <$> readProcess "tput" ["lines"] ""
  screenColumns <- read <$> readProcess "tput" ["cols"] ""
  return ScreenDimensions{..}

terminalSizeWithHelpLine :: IO ScreenDimensions
terminalSizeWithHelpLine = considerHelpLine <$> terminalSize

paginate :: ScreenDimensions -> [T.Text] -> [T.Text]
paginate (ScreenDimensions {..}) ts = mconcat $ fmap T.unlines . pages screenColumns screenRows <$> ts

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
  filenames <- handleArgs >>= eitherToError
  contents <- mapM readFile filenames
  sd <- terminalSizeWithHelpLine
  showPages sd contents $ paginate sd contents
  where handleError :: IOError -> IO ()
        handleError e = putStrLn "Error:" >> print e
        readFile :: FilePath -> IO T.Text
        readFile filename = openFile filename ReadMode >>= TIO.hGetContents
