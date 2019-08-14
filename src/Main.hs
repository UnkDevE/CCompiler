module Main where

import qualified Data.Text as T

import qualified Lexer as L

main :: IO [()]
main = 
    mapM print $ L.tokenize (T.pack "int main() { return 2; }")
