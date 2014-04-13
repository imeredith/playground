{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec
import qualified Data.ByteString as B
import qualified Data.Attoparsec.Char8 as C8
import qualified Data.Attoparsec.Combinator as C
import qualified Data.ByteString.Char8 as B8 (pack)


data Command = Str B.ByteString | Arg B.ByteString deriving Show
cmd :: Parser [Command]
cmd = do
  a <- C8.takeWhile $ \c -> c /= '{'
  b <- "{{" C8..*> C8.takeWhile C8.isAlpha_ascii C8.<*. "}}"
  return [Str a, Arg b]

cmd1 :: Parser [Command]
cmd1 = do
  stuff <- many1 cmd
  re <- many1 C8.anyChar
  return $ concat stuff ++ [Str $ B8.pack re]


main :: IO ()
main = print $ flip fmap $ parseOnly cmd1 "bash /my/script -g\"{{abcd}}\" --stuff={{ad}} mycommand"
