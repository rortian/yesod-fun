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
    entries <- runDB $ selectList [] [Desc SingMapId]
    liftIO $ print entries
    (widget,enctype) <- generateFormPost singMapForm
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Hey!"
        [whamlet|
<table>

  <tr>

    <th>
      id


$forall Entity singMapId singMap <- entries
  <div>#{singMapM singMap}



<div #form>
  This is an example trivial Form. Read the #
  \<a href="http://www.yesodweb.com/book/forms">Forms chapter</a> #
  on the yesod book to learn more about them.
  <form method=post action=@{SingMapsR}#form enctype=#{enctype}>
    ^{widget}
    <input type="submit" value="Send it!">
|]

singMapForm :: Html -> MForm App App (FormResult SingMap, Widget)
singMapForm = renderDivs $ SingMap
    <$> areq intField "m" Nothing
    <*> areq intField "n" Nothing
    <*> areq doubleField "lambdaX" Nothing
    <*> areq doubleField "lambdaY" Nothing

postSingMapsR :: Handler RepHtml
postSingMapsR = do
    ((result, widget), enctype) <- runFormPost singMapForm
    case result of
        FormSuccess res -> do
            entryId <- runDB $ insert res
            redirect $ SingMapR entryId
        _ ->
            defaultLayout $ do
                aDomId <- lift newIdent
                setTitle "Bro, you posted!"
                $(widgetFile "singmap")

getSingMapR :: SingMapId -> Handler RepHtml
getSingMapR singMapId = do
    (singMap) <- runDB $ do
        singMap <- get404 singMapId
        return (singMap)
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Hey!"
        $(widgetFile "singmap_single")
