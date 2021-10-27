import Data.String.Parser

import Language.XML.Element

main : IO ()
main = do
    printLn $ EmptyElem (MkQName Nothing (MkName "br")) []
    printLn $ Elem (MkQName Nothing (MkName "br")) [] [""]

    printLn $ EmptyElem (MkQName Nothing (MkName "img")) [MkAttribute (MkQName Nothing (MkName "align")) "left", MkAttribute (MkQName Nothing (MkName "src")) "https://www.w3.org/Icons/w3c_home"]
    printLn $ Elem (MkQName Nothing (MkName "body")) [] ["", EmptyElem (MkQName Nothing (MkName "hr")) [], ""]
    printLn $ Elem (MkQName Nothing (MkName "p")) [MkAttribute (MkQName Nothing (MkName "class")) "article"] ["Lorem ipsum, dolor", Elem (MkQName Nothing (MkName "em")) [] ["sit"], "amet"]

    let Right (EmptyElem (MkQName Nothing (MkName "br")) [], 5) = parse element "<br/>"
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (Elem (MkQName Nothing (MkName "br")) [] [""], 9) = parse element "<br></br>"
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (
            EmptyElem
                (MkQName Nothing (MkName "img"))
                [MkAttribute (MkQName Nothing (MkName "align")) "left", MkAttribute (MkQName Nothing (MkName "src")) "https://www.w3.org/Icons/w3c_home"],
            59
            ) = parse element #"<img align="left" src="https://www.w3.org/Icons/w3c_home"/>"#
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (
            Elem
                (MkQName Nothing (MkName "body"))
                []
                ["", EmptyElem (MkQName Nothing (MkName "hr")) [], ""],
            24
            ) = parse element
                """
                <body>
                    <hr/>
                </body>
                """
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (
            Elem
                (MkQName Nothing (MkName "p"))
                [MkAttribute (MkQName Nothing (MkName "class")) "article"]
                ["Lorem ipsum, dolor", Elem (MkQName Nothing (MkName "em")) [] ["sit"], "amet"],
            87
            ) = parse element
                    """
                    <p class="article">
                        Lorem ipsum, dolor
                        <em>
                            sit
                        </em>
                        amet
                    </p>
                    """
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    pure ()
