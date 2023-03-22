# Idris2-XML

An XML library for Idris 2.

## Install

Run:

```bash
make install
```

## Usage

Build your code with the `contrib` and `xml` packages, and import the `Language.XML` library.

### Example

```idris
import Data.String.Parser

import Language.XML

main : IO ()
main = do
    printLn someXMLDoc

    putStrLn "---"

    printLn $ mapContent deEmphasize someXMLDoc

    putStrLn "---"

    let Right (anotherXMLDoc, _) = parse xmlDocument
            """
            <?xml version="1.0" encoding="UTF-8" standalone="no"?>
            <!-- Some comment -->
            <?php Some PHP code ?>
            <!DOCTYPE html>
            <!-- Another comment -->
            <p class="article">
                Lorem ipsum, dolor <em>sit</em> amet
            </p>
            <!-- Yet another comment -->
            """
        | err => printLn err

    printLn anotherXMLDoc

  where
    someXMLDoc : XMLDocument
    someXMLDoc = MkXMLDocument
        (MkXMLProlog
            (Just $ MkXMLDecl "1.0" (Just "UTF-8") (Just False))
            [Comment " Some comment ", ProcessingInstruction "php" "Some PHP code "]
            (Just $ MkDocType "html" Nothing)
            [Comment " Another comment "])
        (Elem
            (MkQName Nothing (MkName "p"))
            [MkAttribute (MkQName Nothing (MkName "class")) "article"]
            [" Lorem ipsum, dolor ", Right (Elem (MkQName Nothing (MkName "em")) [] ["sit"]), " amet "])
        [Comment " Yet another comment "]

    deEmphasize : Element -> Element
    deEmphasize = mapContent $ \content => Snd.do
        Right elem <- map (map deEmphasize) content
            | Left misc => pure $ Left misc
        case show elem.name of
            "em" => elem.content
            _ => pure $ Right elem
```
