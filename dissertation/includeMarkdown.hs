import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter includeMarkdown

includeMarkdown :: Block -> IO Block
includeMarkdown cb@(CodeBlock (_,_,namevals)_) = 
    case lookup "include" namevals of
        Just f -> return . RawBlock (Format "markdown") =<< readFile f
        Nothing -> return cb 
includeMarkdown x = return x
