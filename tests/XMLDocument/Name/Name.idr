import Data.String.Parser

import Language.XML.Name

main : IO ()
main = do
    printLn $ MkName "body"
    printLn $ MkQName Nothing (MkName "body")
    printLn $ MkQName (Just $ MkName "html") (MkName "body")

    let Right (MkName "html", 4) = parse name "html"
        | fail => putStrLn "Error parsing XML name, got \{show fail}"

    let Right (MkQName Nothing (MkName "html"), 4) = parse qName "html"
        | fail => putStrLn "Error parsing XML name, got \{show fail}"

    let Right (MkQName (Just $ MkName "xml") (MkName "html"), 8) = parse qName "xml:html"
        | fail => putStrLn "Error parsing XML name, got \{show fail}"

    pure ()
