{-# LANGUAGE OverloadedStrings #-}
module Editor (editor,jsEditor,jsIde,ide,empty) where

import Data.Monoid (mempty)
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)
import qualified System.FilePath as FP

import qualified Elm.Internal.Utils as Elm
import Data.Maybe (fromMaybe)

import Generate (addSpaces)


-- | Display an editor and the compiled result side-by-side.
jsIde :: String -> String -> FilePath -> String -> Html
jsIde cols participant fileName code =
    jsIdeBuilder cols
                 participant
                 ("JS Editor: " ++ FP.takeBaseName fileName)
                 fileName
                 ("/_compile?input=" ++ urlEncode code)

-- | Display an editor and the compiled result side-by-side.
ide :: String -> String -> FilePath -> String -> Html
ide cols participant fileName code =
    ideBuilder cols
               participant
               ("Elm Editor: " ++ FP.takeBaseName fileName)
               fileName
               ("/compile?input=" ++ urlEncode code)

-- | Display an editor and the compiled result side-by-side.
empty :: Html
empty = ideBuilder "50%,50%" "1" "Try Elm" "Empty.elm" "/Try.elm"

jsIdeBuilder :: String -> String -> String -> String -> String -> Html
jsIdeBuilder cols participant title input output =
    H.docTypeHtml $ do
      H.head . H.title . toHtml $ title
      preEscapedToMarkup $ 
         concat [ "<frameset cols=\"" ++ cols ++ "\">\n"
          , "  <frame name=\"input\" src=\"/_code/", input, "?p=", 
            participant, "\" />\n"
          , "  <frame name=\"output\" src=\"", output, "\" />\n"
          , "</frameset>" ]


ideBuilder :: String -> String -> String -> String -> String -> Html
ideBuilder cols participant title input output =
    H.docTypeHtml $ do
      H.head . H.title . toHtml $ title
      preEscapedToMarkup $ 
         concat [ "<frameset cols=\"" ++ cols ++ "\">\n"
                , "  <frame name=\"input\" src=\"/code/", input, "?p=", 
                  participant, "\" />\n"
                , "  <frame name=\"output\" src=\"", output, "\" />\n"
                , "</frameset>" ]

-- | list of themes to use with CodeMirror
themes :: [String]
themes = [ "ambiance", "blackboard", "cobalt", "eclipse"
         , "elegant", "erlang-dark", "lesser-dark", "monokai", "neat", "night"
         , "rubyblue", "solarized", "twilight", "vibrant-ink", "xq-dark" ]

jsFiles :: [AttributeValue]
jsFiles = [ "/codemirror-3.x/lib/codemirror.js"
          , "/codemirror-3.x/mode/elm/elm.js"
          , "/misc/showdown.js"
          , "/misc/editor.js?0.11" ]

jsFiles2 :: [AttributeValue]
jsFiles2 = [ "/codemirror-3.x/lib/codemirror.js"
          , "/codemirror-3.x/mode/javascript/javascript.js"
          , "/misc/showdown.js"
          , "/misc/editor.js?0.11" ]

jsEditor :: FilePath -> String -> Html
jsEditor filePath code =
    H.html $ do
      H.head $ do
        H.title . toHtml $ "JS Editor: " ++ FP.takeBaseName filePath
        H.link ! A.rel "stylesheet" 
               ! A.href "/codemirror-3.x/lib/codemirror.css"
        mapM_ themeAttr themes
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" 
                                    ! A.href "/misc/editor.css"
        mapM_ script jsFiles2
        script "/elm-runtime.js?0.11"
        script "http://cdn.firebase.com/v0/firebase.js"
      H.body $ do
        H.form ! A.id "inputForm" 
               ! A.action "/_compile" 
               ! A.method "post" 
               ! A.target "output" $ do
           H.div ! A.id "editor_box" $
             H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
           H.div ! A.id "options" $ do
             bar "documentation" docs
             bar "editor_options" editorOptions
             bar "always_on" (buttons >> options)
        embed "initEditor();"
  where themeAttr theme = H.link ! A.rel "stylesheet" 
                                 ! A.href (toValue ("/codemirror-3.x/theme/" 
                                                      ++ theme 
                                                      ++ ".css" :: String))
        jsAttr = H.script ! A.type_ "text/javascript"
        script jsFile = jsAttr ! A.src jsFile $ mempty
        embed jsCode = jsAttr $ jsCode

-- | Create an HTML document that allows you to edit and submit Elm code
--   for compilation.
editor :: FilePath -> String -> Html
editor filePath code =
    H.html $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ FP.takeBaseName filePath
        H.link ! A.rel "stylesheet" 
               ! A.href "/codemirror-3.x/lib/codemirror.css"
        mapM_ themeAttr themes
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" 
                                    ! A.href "/misc/editor.css"
        mapM_ script jsFiles
        script "/elm-runtime.js?0.11"
        script "http://cdn.firebase.com/v0/firebase.js"
      H.body $ do
        H.form ! A.id "inputForm" 
               ! A.action "/compile" 
               ! A.method "post" 
               ! A.target "output" $ do
           H.div ! A.id "editor_box" $
             H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
           H.div ! A.id "options" $ do
             bar "documentation" docs
             bar "editor_options" editorOptions
             bar "always_on" (buttons >> options)
        embed "initEditor();"
  where themeAttr theme = H.link ! A.rel "stylesheet" 
                                 ! A.href (toValue ("/codemirror-3.x/theme/" 
                                                      ++ theme 
                                                      ++ ".css" :: String))
        jsAttr = H.script ! A.type_ "text/javascript"
        script jsFile = jsAttr ! A.src jsFile $ mempty
        embed jsCode = jsAttr $ jsCode

bar :: AttributeValue -> Html -> Html
bar id' body = H.div ! A.id id' ! A.class_ "option" $ body

buttons :: Html
buttons = H.div ! A.class_ "valign_kids"
                ! A.style "float:right; padding-right: 6px;"
                $ compileButton
      where
        compileButton = 
            H.input
                 ! A.type_ "button"
                 ! A.id "compile_button"
                 ! A.value "Compile"
                 ! A.onclick "compile()"
                 ! A.title "Ctrl-Enter: change program behavior \
                             \but keep the state"

options :: Html
options = H.div ! A.class_ "valign_kids"
                ! A.style "float:left; padding-left:6px; padding-top:2px;"
                $ (docs' >> opts)
    where 
      docs' = 
        H.span  ! A.title "Show documentation and types." $ "Hints:" >>
            H.input ! A.type_ "checkbox"
                    ! A.id "show_type_checkbox"
                    ! A.onchange "showType(this.checked);"

      opts = 
        H.span  ! A.title "Show editor options." 
                ! A.style "padding-left: 12px;" $ "Options:" >>
            H.input ! A.type_ "checkbox"
                    ! A.id "options_checkbox" 
                    ! A.onchange "showOptions(this.checked);"

editorOptions :: Html
editorOptions = theme >> zoom >> lineNumbers
    where
      optionFor :: String -> Html
      optionFor text =
          H.option ! A.value (toValue text) $ toHtml text

      theme =
          H.select ! A.id "editor_theme"
                   ! A.onchange "setTheme(this.value)"
                   $ mapM_ optionFor themes
              
      zoom =
          H.select ! A.id "editor_zoom"
                   ! A.onchange "setZoom(this.options[this.selectedIndex].\
                                  \innerHTML)"
                   $ mapM_ optionFor ["100%", "80%", "150%", "200%"]

      lineNumbers = do
        H.span ! A.style "padding-left: 16px;" $ "Line Numbers:"
        H.input ! A.type_ "checkbox"
                ! A.id "editor_lines"
                ! A.onchange "showLines(this.checked);"

docs :: Html
docs = tipe >> desc
    where
      tipe = H.div ! A.class_ "type" $ message >> more

      message = H.div ! 
                  A.style "position:absolute; left:4px; right:36px;\
                            \overflow:hidden; text-overflow:ellipsis;" $ ""

      more = H.a ! A.id "toggle_link"
                 ! A.style "display:none; float:right;"
                 ! A.href "javascript:toggleVerbose();"
                 ! A.title "Ctrl+H"
                 $ ""

      desc = H.div ! A.class_ "doc"
                   ! A.style "display:none;"
                   $ ""

