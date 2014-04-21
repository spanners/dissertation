{-# LANGUAGE OverloadedStrings #-}
module Editor (editor,ide,empty) where

import           Data.Monoid                 (mempty)
import           Network.HTTP.Base           (urlEncode)
import qualified System.FilePath             as FP
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Data.Maybe                  (fromMaybe)
import qualified Elm.Internal.Utils          as Elm

import           Generate                    (addSpaces)
import           Utils


-- | Display an editor and the compiled result side-by-side.
ide :: Lang -> String -> String -> FilePath -> String -> Html
ide lang cols participant fileName code =
    case lang of
         Javascript -> buildIde ("JS Editor: ", "/_compile")
         Elm        -> buildIde ("Elm Editor: ", "/compile")
  where buildIde (editStr,compileStr) =
            ideBuilder lang
                       cols
                       participant
                       (editStr ++ FP.takeBaseName fileName)
                       fileName
                       (compileStr ++"?input=" ++ urlEncode code)


-- | Display an editor and the compiled result side-by-side.
empty :: Html
empty = ideBuilder Elm "50%,50%" "1" "Try Elm" "Empty.elm" "/Try.elm"

ideBuilder :: Lang -> String -> String -> String -> String -> String -> Html
ideBuilder lang cols participant title input output =
    case lang of
         Javascript -> makeIde "_code"
         Elm        -> makeIde "code"
  where
        makeIde codeStr =
          H.docTypeHtml $ do
            H.head . H.title . toHtml $ title
            preEscapedToMarkup $
               concat [ "<frameset cols=\"" ++ cols ++ "\">\n"
                      , "  <frame name=\"input\" src=\"/", codeStr, 
                        input, "?p=",
                        participant, "\" />\n"
                      , "  <frame name=\"output\" src=\"", output, "\" />\n"
                      , "</frameset>" ]


-- | list of themes to use with CodeMirror
themes :: [String]
themes = [ "ambiance", "blackboard", "cobalt", "eclipse"
         , "elegant", "erlang-dark", "lesser-dark", "monokai", "neat", "night"
         , "rubyblue", "solarized", "twilight", "vibrant-ink", "xq-dark" ]

jsFiles :: AttributeValue -> [AttributeValue]
jsFiles syntaxFile =
          [ "/codemirror-3.x/lib/codemirror.js"
          , syntaxFile
          , "/misc/showdown.js"
          , "/misc/editor.js?0.11" ]


-- | Create an HTML document that allows you to edit and submit Elm code
--   for compilation.
editor :: Lang -> FilePath -> String -> Html
editor lang filePath code =
    case lang of
         Javascript -> buildEditor ( "JS Editor: "
                                   , "/codemirror-3.x\
                                   \/mode/javascript/javascript.js"
                                   , "/_compile")
         Elm -> buildEditor ( "Elm Editor: "
                            , "/codemirror-3.x/mode/elm/elm.js"
                            , "/compile")
  where buildEditor (editStr, syntaxFile, compileStr) =
          H.html $ do
            H.head $ do
              H.title . toHtml $ editStr ++ FP.takeBaseName filePath
              H.link ! A.rel "stylesheet"
                     ! A.href "/codemirror-3.x/lib/codemirror.css"
              mapM_ themeAttr themes
              H.link ! A.rel "stylesheet" ! A.type_ "text/css"
                                          ! A.href "/misc/editor.css"
              mapM_ script $ jsFiles syntaxFile
              script "/elm-runtime.js?0.11"
              script "http://cdn.firebase.com/v0/firebase.js"
            H.body $ do
              H.form ! A.id "inputForm"
                     ! A.action compileStr
                     ! A.method "post"
                     ! A.target "output" $ do
                 H.div ! A.id "editor_box" $
                   H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
                 H.div ! A.id "options" $ do
                   bar "documentation" docs
                   bar "editor_options" editorOptions
                   bar "always_on" (buttons >> options)
              jsAttr "initEditor();"
        themeAttr theme = H.link ! A.rel "stylesheet"
                                 ! A.href (toValue ("/codemirror-3.x/theme/"
                                                      ++ theme
                                                      ++ ".css" :: String))
        jsAttr = H.script ! A.type_ "text/javascript"
        script jsFile = jsAttr ! A.src jsFile $ mempty

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

