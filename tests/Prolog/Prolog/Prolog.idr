import Data.String.Parser

import Language.XML.Prolog

main : IO ()
main = do
    let decl = Just $ MkXMLDecl "1.0" (Just "UTF-8") (Just False)
    let docType = Just $ MkDocType "html" Nothing

    printLn $ MkXMLProlog Nothing [] Nothing []
    putStrLn "---"
    printLn $ MkXMLProlog decl [Comment " Some comment "] Nothing []
    putStrLn "---"
    printLn $ MkXMLProlog Nothing [] docType [Comment " Some comment "]
    putStrLn "---"
    printLn $ MkXMLProlog decl [Comment " Some comment ", ProcessingInstruction "php" "Some PHP code "] docType [Comment " Another comment "]

    let Right (MkXMLProlog Nothing [] Nothing [], 0) = parse xmlProlog ""
        | fail => putStrLn "Error parsing XML prolog, got \{show fail}"

    let Right (
            MkXMLProlog
                (Just $ MkXMLDecl "1.0" (Just "UTF-8") (Just False))
                [Comment " Some comment "]
                Nothing
                [],
            76
            ) = parse xmlProlog """
                                <?xml version="1.0" encoding="UTF-8" standalone="no"?>
                                <!-- Some comment -->
                                """
        | fail => putStrLn "Error parsing XML prolog, got \{show fail}"

    let Right (
            MkXMLProlog
                Nothing
                []
                (Just $ MkDocType "html" Nothing)
                [Comment " Another comment "],
            40
            ) = parse xmlProlog """
                                <!DOCTYPE html>
                                <!-- Another comment -->
                                """
        | fail => putStrLn "Error parsing XML prolog, got \{show fail}"

    let Right (
            MkXMLProlog
                (Just $ MkXMLDecl "1.0" (Just "UTF-8") (Just False))
                [Comment " Some comment ", ProcessingInstruction "php" "Some PHP code "]
                (Just $ MkDocType "html" Nothing)
                [Comment " Another comment "],
            140
            ) = parse xmlProlog """
                                <?xml version="1.0" encoding="UTF-8" standalone="no"?>
                                <!-- Some comment -->
                                <?php Some PHP code ?>
                                <!DOCTYPE html>
                                <!-- Another comment -->
                                """
        | fail => putStrLn "Error parsing XML prolog, got \{show fail}"

    pure ()
