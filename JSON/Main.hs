module Main (main) where

import JSONMod

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
