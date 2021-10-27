import Data.String.Parser

import Language.XML.CharData

main : IO ()
main = do
    let c = MkCharData ""
    let d = MkCharData "Lorem ipsum"

    printLn c
    printLn d

    printLn c.content
    printLn d.content

    printLn $ the CharData ""
    printLn $ the CharData "Lorem ipsum"

    printLn $ the CharData "" ++ "Lorem" ++ "ipsum" ++ ""

    printLn $ the CharData $ concat []
    printLn $ the CharData $ concat ["", "Lorem", "ipsum", ""]

    let Right (MkCharData "Lorem ipsum", 11) = parse charData "Lorem ipsum"
        | fail => putStrLn "Error parsing XML char data, got \{show fail}"

    let Right (MkCharData "Lorem ipsum, dolor sit amet", 31) = parse charData
            """
            Lorem ipsum,

            dolor  sit   amet
            """
        | fail => putStrLn "Error parsing XML char data, got \{show fail}"

    pure ()
