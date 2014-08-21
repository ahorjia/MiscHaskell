
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- Persist Block
-- share concatenates mkxxxxx results
-- mkPersist turns each entity into a type and a PersistEntity instance for each type
-- sqlSettings sets the "backend" for persist to be SqlBackend
-- mkMigrate creates the table
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistTest = PersistTest ConnectionPool

mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/person/#PersonId PersonR GET
|]

instance Yesod PersistTest

-- The backend was defined as with sqlSettings
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlPersistT

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- List all people in the database
getHomeR :: Handler Html
getHomeR = do
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity personid person <- people
                    <li>
                        <a href=@{PersonR personid}>#{personFirstName person}
        |]

getPersonR :: PersonId -> Handler String
getPersonR personId = do
    -- get404 in yesod.persistence package
    person <- runDB $ get404 personId
    return $ show person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" 26
    warp 3000 $ PersistTest pool
