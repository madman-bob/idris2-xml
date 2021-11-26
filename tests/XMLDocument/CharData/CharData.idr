import Data.String.Parser

import Language.XML.CharData

main : IO ()
main = do
    let c = MkCharData False "" False
    let d = MkCharData False "Lorem ipsum" False

    printLn c
    printLn d

    printLn $ the CharData ""
    printLn $ the CharData "Lorem ipsum"

    let MkCharData False "" False = ""
        | _ => putStrLn #"fromString failed for """#
    let MkCharData True "" False = " "
        | _ => putStrLn #"fromString failed for " ""#
    let MkCharData True "Lorem ipsum, dolor sit amet" True = " Lorem ipsum, dolor sit amet "
        | _ => putStrLn #"fromString failed for " Lorem ipsum, dolor sit amet ""#

    printLn $ the CharData "" ++ "Lorem " ++ " ipsum" ++ ""
    let MkCharData False "Lorem ipsum" False = the CharData "" ++ "Lorem " ++ " ipsum" ++ ""
        | fail => putStrLn "Error concatenating CharData, got \{show fail}"
    let MkCharData True "dolor sit amet" True = the CharData "" ++ " dol" ++ "or " ++ "sit" ++ " amet " ++ ""
        | fail => putStrLn "Error concatenating CharData, got \{show fail}"

    printLn $ the CharData $ concat []
    printLn $ the CharData $ concat ["", "Lorem ", " ipsum", ""]

    let Right (MkCharData False "Lorem ipsum" False, 11) = parse charData "Lorem ipsum"
        | fail => putStrLn "Error parsing XML char data, got \{show fail}"

    let Right (MkCharData False "Lorem ipsum, dolor sit amet" False, 31) = parse charData
            """
            Lorem ipsum,

            dolor  sit   amet
            """
        | fail => putStrLn "Error parsing XML char data, got \{show fail}"

    pure ()
