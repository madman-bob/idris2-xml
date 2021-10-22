import Data.String.Parser

import Language.XML.Element

main : IO ()
main = do
    printLn $ EmptyElem "br" []
    printLn $ Elem "br" [] [Nothing]

    printLn $ EmptyElem "img" [MkAttribute "align" "left", MkAttribute "src" "https://www.w3.org/Icons/w3c_home"]
    printLn $ Elem "body" [] [Nothing, EmptyElem "hr" [], Nothing]
    printLn $ Elem "p" [MkAttribute "class" "article"] [Just "Lorem ipsum, dolor", Elem "em" [] [Just "sit"], Just "amet"]

    let Right (EmptyElem "br" [], 5) = parse element "<br/>"
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (Elem "br" [] [Nothing], 9) = parse element "<br></br>"
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (
            EmptyElem
                "img"
                [MkAttribute "align" "left", MkAttribute "src" "https://www.w3.org/Icons/w3c_home"],
            59
            ) = parse element #"<img align="left" src="https://www.w3.org/Icons/w3c_home"/>"#
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (
            Elem
                "body"
                []
                [Nothing, EmptyElem "hr" [], Nothing],
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
                "p"
                [MkAttribute "class" "article"]
                [Just "Lorem ipsum, dolor", Elem "em" [] [Just "sit"], Just "amet"],
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
