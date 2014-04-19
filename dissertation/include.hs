-- includes.hs
import Text.Pandoc.JSON

import Text.Pandoc.Readers.Native ( readNative )

stripPandocConstructor :: Pandoc -> [Block]
stripPandocConstructor (Pandoc _ blocks) = blocks 

doInclude :: Block -> IO [Block]
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (stripPandocConstructor . readNative) =<< readFile f
       Nothing    -> return [cb]
doInclude x = return [x]

main :: IO ()
main = toJSONFilter doInclude
