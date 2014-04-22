{-# OPTIONS_GHC -W #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import           Control.Applicative
import           Control.Monad.Error
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                    (fromMaybe)
import qualified Elm.Internal.Utils            as Elm

import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeBS
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Regex

import           GHC.Conc
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.Console.CmdArgs
import           System.Directory
import           System.FilePath               as FP
import           System.Process

import qualified Editor
import qualified Elm.Internal.Paths            as Elm
import qualified Generate
import           Utils                         (Lang (..))

data Flags = Flags
  { port :: Int
  } deriving (Data,Typeable,Show,Eq)

flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  }

-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  putStrLn "Initializing Server"
  getRuntimeAndDocs
  setupLogging
  precompile
  cargs <- cmdArgs flags
  httpServe (setPort (port cargs) defaultConfig) $
      ifTop (serveElm "public/Empty.elm")
      <|> route [ ("try", serveHtml Editor.empty)
                , ("edit", edit Elm)
                , ("_edit", edit Javascript)
                , ("code", code Elm)
                , ("_code", code Javascript)
                , ("compile", compile Elm)
                , ("_compile", compile Javascript)
                , ("hotswap", hotswap)
                ]
      <|> serveDirectoryWith directoryConfig "public/build"
      <|> serveDirectoryWith simpleDirectoryConfig "resources"
      <|> error404

error404 :: Snap ()
error404 =
    do modifyResponse $ setResponseStatus 404 "Not found"
       serveElm "public/build/Error404.elm"

serveElm :: FilePath -> Snap ()
serveElm = serveFileAs "text/html; charset=UTF-8"

logAndServeJS :: MonadSnap m => H.Html -> m ()
logAndServeJS = serveHtml

logAndServeHtml :: MonadSnap m => (H.Html, Maybe String) -> m ()
logAndServeHtml (html, Nothing)  = serveHtml html
logAndServeHtml (html, Just err) =
    do timeStamp <- liftIO $ readProcess "date" ["--rfc-3339=ns"] ""
       liftIO $ appendFile "error_log.json" $ "{\"" ++ init timeStamp
                                                    ++ "\","
                                                    ++ show (lines err)
                                                    ++ "},"
       setContentType "text/html" <$> getResponse
       writeLBS (BlazeBS.renderHtml html)

embedHtml :: MonadSnap m => H.Html -> Lang -> String -> m ()
embedHtml html lang participant =
    do elmSrc <- liftIO $ readFile "EmbedMe.elm"
       setContentType "text/html" <$> getResponse
       writeLBS (BlazeBS.renderHtml (embedMe lang elmSrc html participant))

serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
    do setContentType "text/html" <$> getResponse
       writeLBS (BlazeBS.renderHtml html)

hotswap :: Snap ()
hotswap = maybe error404 serve =<< getParam "input"
    where
      serve code =
          do setContentType "application/javascript" <$> getResponse
             writeBS . BSC.pack . Generate.js $ BSC.unpack code

compile :: Lang -> Snap ()
compile lang = maybe error404 serve =<< getParam "input"
    where
      serve = case lang of
                   Elm -> logAndServeHtml
                            . Generate.logAndHtml "Compiled Elm"
                            . BSC.unpack
                   Javascript -> logAndServeJS
                                   . Generate.logAndJS "Compiled JS"
                                   . BSC.unpack

edit :: Lang -> Snap ()
edit lang = do
  participant <- BSC.unpack . fromMaybe "" <$> getParam "p"
  cols <- BSC.unpack . fromMaybe "50%,50%" <$> getQueryParam "cols"
  withFile (Editor.ide lang cols participant)

code :: Lang -> Snap ()
code lang = do
  participant <- BSC.unpack . fromMaybe "" <$> getParam "p"
  embedWithFile Editor.editor lang participant

embedee :: Lang -> String -> String -> H.Html
embedee lang elmSrc participant =
    H.span $ do
      case Elm.compile elmSrc of
        Right jsSrc ->
            jsAttr $ H.preEscapedToMarkup (subRegex oldID jsSrc newID)
        Left err ->
            H.span ! A.style "font-family: monospace;" $
            mapM_ (\line ->
                      H.preEscapedToMarkup
                      (Generate.addSpaces line)
                      >> H.br)
                  (lines err)
      jsAttr $ H.preEscapedToMarkup $ visualiser
      where langStr = (case lang of 
                            Elm -> "elm"
                            Javascript -> "js")
            visualiser =
              concat [ "var firebaseData = new Firebase('"
                      , "http://sweltering-fire-9141.firebaseio.com/"
                      , "dissertation/"
                      , langStr
                      , "/"
                      , participant
                      , "');"
                      , "var elm = Elm.fullscreen(Elm.EmbedMe, {"
	                  , "stamped: {"
		              , "      t: 0,"
		              , "      x: 0,"
		              , "      y: 0"
	                  , "  }"
                      , "});"
                      , "firebaseData.on('child_added'," 
                      , "function(snapshot) {"
	                  , "elm.ports.stamped.send(snapshot.val());"
                      , "});" ]
            oldID = mkRegex "var user_id = \"1\";"
            newID = "var user_id = \"" ++ langStr 
                                       ++ "/" 
                                       ++ participant 
                                       ++ "\";"
            jsAttr = H.script ! A.type_ "text/javascript"

embedMe :: Lang -> String -> H.Html -> String -> H.Html
embedMe lang elmSrc target participant = target >> embedee
                                                   lang
                                                   elmSrc
                                                   participant

embedWithFile :: (Lang -> FilePath -> String -> H.Html) -> Lang
                                                        -> String
                                                        -> Snap ()
embedWithFile handler lang participant = do
  path <- BSC.unpack . rqPathInfo <$> getRequest
  let file = "public/" ++ path
  exists <- liftIO (doesFileExist file)
  if not exists then error404 else
      do content <- liftIO $ readFile file
         embedHtml (handler lang path content) lang participant

withFile :: (FilePath -> String -> H.Html) -> Snap ()
withFile handler = do
  path <- BSC.unpack . rqPathInfo <$> getRequest
  let file = "public/" ++ path
  exists <- liftIO (doesFileExist file)
  if not exists then error404 else
      do content <- liftIO $ readFile file
         serveHtml $ handler path content

directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexGenerator = defaultIndexGenerator 
                         defaultMimeTypes 
                         indexStyle
    , mimeTypes = Map.insert ".elm" "text/html" defaultMimeTypes
    }

indexStyle :: BS.ByteString
indexStyle =
    "body { margin:0; font-family:sans-serif; \
    \       background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \div.footer { display:none; }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"

setupLogging :: IO ()
setupLogging =
    do createDirectoryIfMissing True "log"
       createIfMissing "log/access.log"
       createIfMissing "log/error.log"
    where
      createIfMissing path = do
        exists <- doesFileExist path
        unless exists $ BS.writeFile path ""

-- | Compile all of the Elm files in public/, 
--   placing results in public/build/
precompile :: IO ()
precompile =
  do setCurrentDirectory "public"
     files <- getFiles True ".elm" "."
     forM_ files $ \file ->
                     rawSystem "elm" [ "--make"
                                     , "--runtime=/elm-runtime.js"
                                     , file ]
     htmls <- getFiles False ".html" "build"
     mapM_ adjustHtmlFile htmls
     setCurrentDirectory ".."
  where
    getFiles :: Bool -> String -> FilePath -> IO [FilePath]
    getFiles skip ext directory =
        if skip && "build" `elem` map FP.dropTrailingPathSeparator
                                      (FP.splitPath directory)
          then return [] else
          (do contents <- map (directory </>) `fmap`
                                                getDirectoryContents
                                                directory
              let files = filter ((ext==) . FP.takeExtension) contents
                  directories  = filter (not . FP.hasExtension) contents
              filess <- mapM (getFiles skip ext) directories
              return (files ++ concat filess))

getRuntimeAndDocs :: IO ()
getRuntimeAndDocs = do
  writeFile "resources/elm-runtime.js" =<< readFile Elm.runtime
  writeFile "resources/docs.json" =<< readFile Elm.docs

adjustHtmlFile :: FilePath -> IO ()
adjustHtmlFile file =
  do src <- BSC.readFile file
     let (before, after) = BSC.breakSubstring "<title>" src
     BSC.writeFile (FP.replaceExtension file "elm") $
        BSC.concat [before, style, after]
     removeFile file

style :: BSC.ByteString
style =
    "<style type=\"text/css\">\n\
    \  a:link {text-decoration: none; color: rgb(15,102,230);}\n\
    \  a:visited {text-decoration: none}\n\
    \  a:active {text-decoration: none}\n\
    \  a:hover {text-decoration: underline; color: rgb(234,21,122);}\n\
    \  body { font-family: \"Lucida Grande\",\
    \ \"Trebuchet MS\",\"Bitstream Vera Sans\",Verdana,Helvetica,\
    \ sans-serif !important; }\n\
    \  p, li { font-size: 14px !important;\n\
    \          line-height: 1.5em !important; }\n\
    \</style>"
