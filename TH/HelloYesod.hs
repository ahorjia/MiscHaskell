
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Yesod

-- Foundation data type
data HelloWorld = HelloWorld

-- Front Controller pattern
-- Routing
-- mkYesod Template Haskell
-- parseRoutes is a Quasi-quoter
-- Declarative style definition of routes
-- Root / + RouteName + Type of Request
-- HomeR resource/convention/RouteName
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

-- Foundation data type
instance Yesod HelloWorld

-- Handler funtion get (GEt) + HomeR (resource)

getHomeR :: Handler Html
-- defaultLayout to generate the response (Html content)
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
