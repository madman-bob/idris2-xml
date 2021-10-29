module Language.XML

import Data.String
import Data.String.Extra
import Data.String.Parser
import Data.List

import public Language.XML.Element
import public Language.XML.Misc
import public Language.XML.Prolog

public export
record XMLDocument where
    constructor MkXMLDocument
    prolog : XMLProlog
    root : Element
    misc : List Misc

%name XMLDocument doc

public export
mapContent : (Element -> Element) -> XMLDocument -> XMLDocument
mapContent f (MkXMLDocument prolog root misc) = MkXMLDocument prolog (f root) misc

export
Show XMLDocument where
    show doc = join "\n" $ filter (/= "") $
        show doc.prolog :: show doc.root :: map show doc.misc

export
xmlDocument : Parser XMLDocument
xmlDocument = (do
    pure $ MkXMLDocument
        !xmlProlog
        !(spaces *> element)
        !(many $ spaces *> misc)
  ) <?> "XML document"
