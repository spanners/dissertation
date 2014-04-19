#!/usr/bin/env runhaskell
-- includes.hs
import Text.Pandoc.JSON
import Text.Pandoc.Definition

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (Plain . map Str . lines) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

main :: IO ()
main = toJSONFilter doInclude
