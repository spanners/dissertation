-- includeCode.hs
import Text.Pandoc.JSON

includeCode :: Block -> IO Block
includeCode cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "code" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
includeCode x = return x

main :: IO ()
main = toJSONFilter includeCode
