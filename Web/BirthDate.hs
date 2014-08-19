
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Arrows                #-}
import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text, pack)
import           Data.Time           (Day)
import           Yesod
import           Yesod.Form.Jquery
import           Control.Arrow

data AgeForm = AgeForm

mkYesod "AgeForm" [parseRoutes|
/ HomeR GET
/age AgeR POST
|]

instance Yesod AgeForm

instance RenderMessage AgeForm FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery AgeForm

data Age = Age
    { year :: Int
    , month :: Month 
    , day :: Int
    , hour :: Maybe Int
    , minute :: Maybe Int
    }
    deriving Show

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving(Show, Eq, Enum, Bounded)

ageAForm :: Maybe Age -> AForm Handler Age
ageAForm mage = Age
    <$> areq (selectFieldList years) "Year" (year <$> mage)
    <*> areq (selectFieldList months) "Month" (month <$> mage)
    <*> areq intField "Day" (day <$> mage)
    <*> aopt intField "Hour" (hour <$> mage)
    <*> aopt intField "Minute" (minute <$> mage)
    where
        years :: [(Text, Int)]
        years = map (pack . show &&& id) [1900..2014]
        months :: [(Text, Month)]
        months = map (pack . show &&& id) $ [minBound..maxBound]
        
ageForm :: Html -> MForm Handler (FormResult Age, Widget)
ageForm = renderTable $ ageAForm $ Just $ Age 2013 Mar 27 (Just 9) (Just 45)

getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost ageForm
    defaultLayout
        [whamlet|
            <p>
                The widget generated contains only the contents
                of the form, not the form tag itself. So...
            <form method=post action=@{AgeR} enctype=#{enctype}>
               ^{widget}
               <p>It also doesn't include the submit button.
               <button>Submit
        |]

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postAgeR :: Handler Html
postAgeR = do
    ((result, widget), enctype) <- runFormPost ageForm
    case result of
        FormSuccess age -> defaultLayout [whamlet|<p>#{show age}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{AgeR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

main :: IO ()
main = warp 3000 AgeForm
