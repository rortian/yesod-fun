{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

getSingMapsR :: Handler RepHtml
getSingMapsR = do
    (widget,enctype) <- generateFormPost singMapForm
    let handlerName = "getSigMapsR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Hey!"
        $(widgetFile "singmap")

singMapForm :: Html -> MForm App App (FormResult SingMap, Widget)
singMapForm = renderDivs $ SingMap
    <$> areq intField "m" Nothing
    <*> areq intField "n" Nothing
    <*> areq doubleField "lambdaX" Nothing
    <*> areq doubleField "lambdaY" Nothing

postSingMapsR :: Handler RepHtml
postSingMapsR = do
    ((result, widget), enctype) <- runFormPost singMapForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Bro, you posted!"
        $(widgetFile "singmap")
