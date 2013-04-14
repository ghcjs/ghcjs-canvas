module Main where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import System.IO

main = do
  ls0 <- lines <$> readFile "../JavaScript/Canvas.hs"
  let ls = filter ("foreign import javascript" `isPrefixOf`) (combineLines ls0)
  h <- openFile "../JavaScript/nonGhcjsStubs.txt" WriteMode
  forM_ ls $ \l -> do
    let sig = filter ("js_" `isPrefixOf`) (tails l)
    case sig of
      []    -> return ()
      (x:_) -> do
        let fun = head (words x)
        hPutStrLn h (unwords . words $ x)
        hPutStrLn h (fun ++ " = error \"" ++ fun ++ ": only available in JavaScript\"")
  hClose h

combineLines :: [String] -> [String]
combineLines (x:y:xs)
  | "foreign import javascript" `isPrefixOf` x &&
    "js_" `isPrefixOf` (dropWhile isSpace y) = (x ++ " " ++ dropWhile isSpace y) : combineLines xs
  | otherwise = x : combineLines (y:xs)
combineLines xs = xs

