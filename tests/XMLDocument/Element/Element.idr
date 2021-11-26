import Data.String.Parser

import Language.XML.Element

main : IO ()
main = do
    printLn $ EmptyElem (MkQName Nothing (MkName "br")) []
    printLn $ Elem (MkQName Nothing (MkName "br")) [] [""]

    let img = EmptyElem (MkQName Nothing (MkName "img")) [MkAttribute (MkQName Nothing (MkName "align")) "left", MkAttribute (MkQName Nothing (MkName "src")) "https://www.w3.org/Icons/w3c_home"]
    let body = Elem (MkQName Nothing (MkName "body")) [] ["", EmptyElem (MkQName Nothing (MkName "hr")) [], ""]
    let p = Elem (MkQName Nothing (MkName "p")) [MkAttribute (MkQName Nothing (MkName "class")) "article"] [" Lorem ipsum, dolor ", Elem (MkQName Nothing (MkName "em")) [] ["sit"], " amet "]

    printLn img
    printLn body
    printLn p

    putStrLn $ textContent img
    putStrLn $ textContent p

    printLn $ lookup (MkQName Nothing $ MkName "src") img.attrs
    printLn $ find (\elem => elem.name == MkQName Nothing (MkName "em")) p

    printLn $ deEmphasize p -- Tests mapContent
    printLn $ maybeDeEmphasize True p -- Tests mapContentM
    printLn $ maybeDeEmphasize False p -- Tests mapContentM

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
            18
            ) = parse element
                """
                <body><hr/></body>
                """
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    let Right (
            Elem
                (MkQName Nothing (MkName "p"))
                [MkAttribute (MkQName Nothing (MkName "class")) "article"]
                [" Lorem ipsum, dolor ", Elem (MkQName Nothing (MkName "em")) [] ["sit"], " amet "],
            73
            ) = parse element
                    """
                    <p class="article">
                        Lorem ipsum, dolor
                        <em>sit</em>
                        amet
                    </p>
                    """
        | fail => putStrLn "Error parsing XML element, got \{show fail}"

    pure ()
  where
    deEmphasize : Element -> Element
    deEmphasize = mapContent \content => Snd.do
        elem <- map deEmphasize content
        case show elem.name of
            "em" => elem.content
            _ => pure elem

    maybeDeEmphasize : Bool -> Element -> Maybe Element
    maybeDeEmphasize ok = mapContentM \contents => (do
        Just elem <- Just $ map (maybeDeEmphasize ok) contents
            | Nothing => Nothing
        case (ok, show elem.name) of
             (False, "em") => Nothing
             (True, "em") => Just $ elem.content
             (_, _) => Just $ pure elem
      ) @{Compose @{(%search, SndMonad, %search)}}
