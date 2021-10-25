import Data.String.Parser

import Language.XML.Element

main : IO ()
main = do
    printLn $ EmptyElem (MkQName Nothing (MkName "br")) []
    printLn $ Elem (MkQName Nothing (MkName "br")) [] [Nothing]

    printLn $ EmptyElem (MkQName Nothing (MkName "img")) [MkAttribute (MkQName Nothing (MkName "align")) "left", MkAttribute (MkQName Nothing (MkName "src")) "https://www.w3.org/Icons/w3c_home"]
    printLn $ Elem (MkQName Nothing (MkName "body")) [] [Nothing, EmptyElem (MkQName Nothing (MkName "hr")) [], Nothing]
    printLn $ Elem (MkQName Nothing (MkName "p")) [MkAttribute (MkQName Nothing (MkName "class")) "article"] [Just "Lorem ipsum, dolor", Elem (MkQName Nothing (MkName "em")) [] [Just "sit"], Just "amet"]

    let Right (EmptyElem (MkQName Nothing (MkName "br")) [], 5) = parse element "<br/>"
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (Elem (MkQName Nothing (MkName "br")) [] [Nothing], 9) = parse element "<br></br>"
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
                [Nothing, EmptyElem (MkQName Nothing (MkName "hr")) [], Nothing],
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
                [Just "Lorem ipsum, dolor", Elem (MkQName Nothing (MkName "em")) [] [Just "sit"], Just "amet"],
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
