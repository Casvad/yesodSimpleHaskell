{-# LANGUAGE 
    TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, 
    OverloadedStrings,ViewPatterns 
#-}



module Site
    ( myFirstSite
    ) where

import Yesod
import qualified Data.Text as T

data App = App

instance Yesod App where 

mkYesod "App" [parseRoutes|
/               HomeR    GET
/echo/#String     EchoR    GET
/reverse/#T.Text     Reverse  GET
|]

getHomeR :: Handler Html
getHomeR = do
   defaultLayout [whamlet|
         <h1>Hello World
|]

getEchoR :: String -> Handler Html
getEchoR str = do
        mid <- lookupGetParam "id"
        defaultLayout [whamlet|
        <h1> #{str}
        $maybe id <- mid
                 <h2>id: #{id}
        $nothing
                 <h2>No hay id
        <a href=@{Reverse (T.pack str)}>toReverse
        <a href=@{HomeR}>toHome
|]

getReverse :: T.Text -> Handler Html
getReverse str = do 
        defaultLayout [whamlet|
        <h1> #{T.reverse str}
|]


myFirstSite :: IO ()
myFirstSite = warp 3000 App
