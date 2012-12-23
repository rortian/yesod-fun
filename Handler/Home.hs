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
    redirect SingMapsR

getGridsR :: Handler RepHtml
getGridsR = do
    (widget,enctype) <- generateFormPost gridForm
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "The grids are alive"
        $(widgetFile "grid")

postGridsR :: Handler RepHtml
postGridsR = do
    redirect SingMapsR

gridForm :: Html -> MForm App App (FormResult Grid, Widget)
gridForm = renderDivs $ Grid
    <$> areq intField "width" Nothing
    <*> areq intField "height" Nothing
    <*> areq doubleField "scale" Nothing
    <*> lmaps
    where
        lmaps = areq (selectField maps) "SingMap" Nothing
            where
                maps :: GHandler App App (OptionList SingMapId)
                maps = do
                    entities <- runDB $ selectList [] [Desc SingMapId]
                    optionsPairs $ map (\singmap -> (entityVal singmap,entityKey singmap)) entities

getGridR :: GridId -> Handler RepHtml
getGridR gridId = do
    (grid) <- runDB $ do
        grid <- get404 gridId
        return (grid)
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Nice Grid"
        $(widgetFile "grid_single")


getSingMapsR :: Handler RepHtml
getSingMapsR = do
    entries <- runDB $ selectList [] [Desc SingMapId]
    liftIO $ print entries
    (widget,enctype) <- generateFormPost singMapForm
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Hey!"
        [whamlet|
<table width="80%">

  <tr>

    <tbody align="center">

      $forall Entity singMapId singMap <- entries
        <tr>
          <td>#{singMapM singMap}
          <td>#{singMapN singMap}
          <td>#{singMapLambdaX singMap} + #{singMapLambdaY singMap}i
          <td><a href="@{SingMapR singMapId}">Link!</a>
    <thead>

      <th>
        m

      <th>
        n

      <th>
        lambda

      <th>
        link



<div #form>
  Enter a new map
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
