
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import           Data.Text           (Text, pack, unpack)
import           Yesod.Form.Jquery
import Yesod
import Control.Arrow --Fanout operator
import Control.Applicative ((<$>),(<*>)) --fmap apply and lift

import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- Foundation data type
data HelloWorld = HelloWorld ConnectionPool

-- Watch for the comments in the block
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
/age AgeR POST
|]

-- Foundation data type
instance Yesod HelloWorld

instance RenderMessage HelloWorld FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Handler funtion get (GEt) + HomeR (resource)
getPage1R :: Handler Html
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}> Go to page 2|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}> Go to Home|]

--The Form Data

getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    -- widgets are glues
    -- enctype encoding type UrlEncoded
    -- generate the form, ignore the post parameters
    (widget, enctype) <- generateFormPost ageForm
    let authorsName = ("San Diego Haskell Users Group" :: Text)
    let condition = True
    defaultLayout $ do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        toWidget [lucius| h1 { color: green; } |]
        toWidget [julius|
            $(function() {
                $("h1").click(function() {
                    alert("You clicked on the heading!");
                });
            });
        |]
        -- Hamlet is the HTMl templating language
        -- Shakespearean languages (Hamlet, Cassiums, Lucius, Julius)
        -- Hamlet indentation for nesting instead of closing tags
        -- varialbe interpolation #
        -- whamlet converts Hamlet syntax to a widget (shamlet)
        -- whamlet is like hamlet but it can embed a widget
        toWidget [whamlet|
            Hello World!i
            <br>
            $if condition
              <h1>#{authorsName}
            $else
              <h2>No user group
            <a href=@{Page1R}>Go to page1
            <p>Some text in p
            <form method=post action=@{AgeR} enctype=#{enctype}>
               ^{widget}
               <button>Submit
        |]

postAgeR :: Handler Html
postAgeR = do
    -- runFormPost: Run the form against submitted Post parameters
    -- typeclass Enctype defines ToHtml can pass to hamlet
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

-- Persist Block
-- share concatenates mkxxxxx results
-- mkPersist turns each entity into a type and a PersistEntity instance for each type
-- sqlSettings sets the "backend" for persist to be SqlBackend
-- mkMigrate creates the table
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person 
    name Text
    year Int
    month Month
    day Maybe Int
|]

instance Show Person where
    show (Person n y m d) = 
        "Hello: " ++ (unpack n)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving(Show, Eq, Enum, Bounded)

-- Applicative/Monadic/Input forms
-- Applicative most common
-- Monadic most flexible
-- Input?
-- Split the applicative and monadic forms
ageAForm :: Maybe Person -> AForm Handler Person 
ageAForm mperson = Person
    -- areq and aopt functions for function and applicative
    <$> areq textField "Name" (name <$> mperson)
    <*> areq (selectFieldList years) "Year" (year <$> mperson)
    <*> areq (selectFieldList months) "Month" (month <$> mperson)
    <*> aopt ageDayField "Day" (day <$> mperson) -- intField
    where
        years :: [(Text, Int)] -- Fanout operator Control.Arrow
        years = map (pack . show &&& id) [1900..2014] --for selectFieldList
        months :: [(Text, Month)]
        months = map (pack . show &&& id) $ [minBound..maxBound]

        errorMessage :: Text
        errorMessage = "Invalid day!"

        ageDayField = check validateAgeDayField intField

        validateAgeDayField d
         | d < 0 = Left errorMessage
         | d > 31 = Left errorMessage
         | otherwise = Right d
        
-- renderTable calls aformToForm which converts applicative form
-- to monadic form
ageForm :: Html -> MForm Handler (FormResult Person, Widget)
ageForm = renderTable $ ageAForm $ Just $ Person "John" 2013 Mar (Just 27)

main :: IO ()
main = withSqlitePool "testDemo.db3" 10 $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "SD HUG1" 2014 Mar (Just 12)
    warp 3000 $ HelloWorld pool


-- The backend was defined as with sqlSettings
instance YesodPersist HelloWorld  where
    type YesodPersistBackend HelloWorld = SqlPersistT

    runDB action = do
        HelloWorld pool <- getYesod
        runSqlPool action pool

