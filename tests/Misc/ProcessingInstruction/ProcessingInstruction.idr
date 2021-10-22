import Data.String.Parser

import Language.XML.Misc

main : IO ()
main = do
    printLn $ ProcessingInstruction "php" "Some PHP code "

    let Right (ProcessingInstruction "php" "Some PHP code ", 22) = parse processingInstruction "<?php Some PHP code ?>"
        | fail => putStrLn "Error parsing processing instruction, got \{show fail}"

    pure ()
