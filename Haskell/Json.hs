{-# LANGUAGE OverloadedStrings #-}
module DataSource where

import Data.Either
import Data.Maybe

import Data.Aeson
import Data.Aeson.Types

import qualified Data.Text.Internal as T
import Control.Applicative

import qualified Data.ByteString.Lazy.Char8 as B

-- ouputs "Just 9"
myjson :: Maybe Int
myjson = do
  json <- decode $ "{\"key\":5, \"keys\":{\"blah\":4}}"
  flip parseMaybe json $ \obj -> (++) <$>
                                 get_value ["key"] obj <*>
                                 get_value ["keys","blah"] obj))



get_value :: FromJSON a => [T.Text] -> Object -> Parser a
get_value [x] = (.: x)
get_value (x:xs) = \obj -> (obj .: x) >>= (get_value xs)


get_int_value :: [T.Text] -> Object -> Parser Int
get_int_value = get_value

get_string_value :: [T.Text] -> Object -> Parser String
get_string_value = get_value

