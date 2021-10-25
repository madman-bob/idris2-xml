import Data.String.Parser

import Language.XML

main : IO ()
main = do
    printLn $ MkXMLDocument
        (MkXMLProlog Nothing [] Nothing [])
        (Elem
            (MkQName Nothing (MkName "p"))
            [MkAttribute (MkQName Nothing (MkName "class")) "article"]
            [Just "Lorem ipsum, dolor sit amet"])
        []

    putStrLn "---"

    printLn $ MkXMLDocument
        (MkXMLProlog
            (Just $ MkXMLDecl "1.0" (Just "UTF-8") (Just False))
            [Comment " Some comment ", ProcessingInstruction "php" "Some PHP code "]
            (Just $ MkDocType "html" Nothing)
            [Comment " Another comment "])
        (Elem
            (MkQName Nothing (MkName "p"))
            [MkAttribute (MkQName Nothing (MkName "class")) "article"]
            [Just "Lorem ipsum, dolor", Elem (MkQName Nothing (MkName "em")) [] [Just "sit"], Just "amet"])
        [Comment " Yet another comment "]

    let Right (
            MkXMLDocument
                (MkXMLProlog Nothing [] Nothing [])
                (Elem
                    (MkQName Nothing (MkName "p"))
                    [MkAttribute (MkQName Nothing (MkName "class")) "article"]
                    [Just "Lorem ipsum, dolor sit amet"])
                [],
            50
            ) = parse xmlDocument #"<p class="article">Lorem ipsum, dolor sit amet</p>"#
        | fail => putStrLn "Error parsing XML document, got \{show fail}"

    let Right (
            MkXMLDocument
                (MkXMLProlog
                    (Just $ MkXMLDecl "1.0" (Just "UTF-8") (Just False))
                    [Comment " Some comment ", ProcessingInstruction "php" "Some PHP code "]
                    (Just $ MkDocType "html" Nothing)
                    [Comment " Another comment "])
                (Elem
                    (MkQName Nothing (MkName "p"))
                    [MkAttribute (MkQName Nothing (MkName "class")) "article"]
                    [Just "Lorem ipsum, dolor", Elem (MkQName Nothing (MkName "em")) [] [Just "sit"], Just "amet"])
                [Comment " Yet another comment "],
            229
            ) = parse xmlDocument
                    """
                    <?xml version="1.0" encoding="UTF-8" standalone="no"?>
                    <!-- Some comment -->
                    <?php Some PHP code ?>
                    <!DOCTYPE html>
                    <!-- Another comment -->
                    <p class="article">Lorem ipsum, dolor <em>sit</em> amet</p>
                    <!-- Yet another comment -->
                    """
        | fail => putStrLn "Error parsing XML document, got \{show fail}"

    pure ()
