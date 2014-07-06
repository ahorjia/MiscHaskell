module PutJSON where

import Data.List (intercalate)
import JSONMod

renderJValue :: JValue -> Doc 

renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber n) = double num
renderJValue (JString s) = string s

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
	      pairs ps = intercalate ", " (map renderPair ps)
				renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
	      values vs = intercalate ", " (map renderJValue vs)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
