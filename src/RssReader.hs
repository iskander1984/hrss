-----------------------------------------------------------------------------
--
-- Module      :  RssReader
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Oleksandr Pochapskyy
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module RssReader  where
import Network.HTTP
import Control.Applicative
import Data.Maybe
import Network.URI
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows

data FeedItem = FeedItem { guid, title, link, subhead, description::String} deriving Show

data Feed = Feed { channelTitle::String }

instance Show Feed where
    show feed = show $ channelTitle feed

parseXML doc = readString [ withValidate no
                          , withRemoveWS yes
                          ] doc

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = atTag tag >>> text

retrieveSubscriptionTitles :: String -> IO [Feed]
retrieveSubscriptionTitles url = do
    content <- retrieveContentFromUrl url
    xml     <- return $ parseXML content
    result  <- runX (xml >>> getFeedTitle)
    return result

retrieveContentFromUrl :: String -> IO String
retrieveContentFromUrl url =
    do Network.HTTP.simpleHTTP request >>= getResponseBody
    where request = Request { rqURI = uri,
                              rqMethod = GET,
                              rqHeaders = [],
                              rqBody = ""}
          uri = fromJust $ parseURI url

getFeedTitle = getXPathTrees "//channel/title" >>>
  proc x -> do
    title     <- text           -< x
    returnA                     -< Feed
      { channelTitle = title}

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success.
downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r ->
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadURL url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url

-}

