
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
/page1 Page1R GET
/page2 Page2R GET
|]

-- Foundation data type
instance Yesod HelloWorld

-- Handler funtion get (GEt) + HomeR (resource)

getHomeR :: Handler Html
-- Page1R data constructor enables type-safe URLs
getHomeR = defaultLayout [whamlet|Hello World!<br><a href=@{Page1R}>Go to page1|]

getPage1R :: Handler Html
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}> Go to page 2|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}> Go to Home|]
main :: IO ()
main = warp 3000 HelloWorld
