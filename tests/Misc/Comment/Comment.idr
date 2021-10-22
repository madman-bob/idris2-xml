import Data.String.Parser

import Language.XML.Misc

main : IO ()
main = do
    printLn $ Comment " Some comment "

    let Right (Comment " Some comment ", 21) = parse comment "<!-- Some comment -->"
        | fail => putStrLn "Error parsing comment, got \{show fail}"

    pure ()
