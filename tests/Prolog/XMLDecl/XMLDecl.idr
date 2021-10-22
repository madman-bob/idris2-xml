import Data.String.Parser

import Language.XML.Prolog.XMLDecl

main : IO ()
main = do
    printLn $ MkXMLDecl "1.0" Nothing Nothing
    printLn $ MkXMLDecl "1.0" (Just "UTF-8") (Just False)

    let Right (MkXMLDecl "1.0" Nothing Nothing, 21) = parse xmlDecl #"<?xml version="1.0"?>"#
        | fail => putStrLn "Error parsing XML declaration, got \{show fail}"

    let Right (MkXMLDecl "1.0" (Just "UTF-8") (Just False), 54) = parse xmlDecl #"<?xml version="1.0" encoding="UTF-8" standalone="no"?>"#
        | fail => putStrLn "Error parsing XML declaration, got \{show fail}"

    pure ()
