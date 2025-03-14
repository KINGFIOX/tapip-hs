module Main where

import qualified Tapip (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Tapip.someFunc
