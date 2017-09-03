-- | Renders the callgraph of a given file (by its Kythe node id) into
-- graphviz (dot) format.
module Main where

import Control.Monad (forM_)
import Data.Text (pack, unpack)
import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  -- example: kythe:corpus?path=some/path/to.hs
  (fileNode:_) <- getArgs
  pairs <- runCayley (pack fileNode)
  putStrLn "digraph G {"
  putStrLn "rankdir = BT;" -- potential factor candidates appear on top.
  forM_ pairs $ \(src, dst) -> putStrLn (unpack src ++ " -> " ++ unpack dst)
  putStrLn "}"
