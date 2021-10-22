import Data.String.Parser

import Language.XML.Prolog.DocType

main : IO ()
main = do
    printLn $ MkDocType "html" Nothing
    printLn $ MkDocType "html" (Just $ Public "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")

    let Right (MkDocType "html" Nothing, 15) = parse docType "<!DOCTYPE html>"
        | fail => putStrLn "Error parsing XML document type, got \{show fail}"

    let Right (MkDocType "html" (Just $ Public "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"), 97)
            = parse docType #"<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">"#
        | fail => putStrLn "Error parsing XML document type, got \{show fail}"

    pure ()
