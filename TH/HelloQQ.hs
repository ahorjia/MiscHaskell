
{-# LANGUAGE TemplateHaskell #-}

module HelloQuoter  where
import Language.Haskell.TH.Quote
import Language.Haskell.TH

helloQQ :: QuasiQuoter
helloQQ = QuasiQuoter { qExp = \_ -> [| "Hello World" |] }
