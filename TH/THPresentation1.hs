
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

main = sampleExp >>= putStrLn.pprint
sampleExp = do  x <- newName "x"
		return (LamE [VarP x] (LitE (StringL "hello world")))
