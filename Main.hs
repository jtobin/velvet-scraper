
module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.Environment
import Velvet.Scraper (vencode)

main :: IO ()
main = do
  assemblies <- getArgs
  ByteString.putStrLn =<< vencode assemblies

