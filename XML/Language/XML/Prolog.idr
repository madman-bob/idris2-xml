module Language.XML.Prolog

import Data.String.Extra
import Data.String.Parser

import public Language.XML.Misc
import public Language.XML.Prolog.DocType
import public Language.XML.Prolog.XMLDecl

public export
record XMLProlog where
    constructor MkXMLProlog
    xmlDecl : Maybe XMLDecl
    xmlDeclMisc : List Misc
    docType : Maybe DocType
    docTypeMisc : List Misc

%name XMLProlog prolog

export
Show XMLProlog where
    show prolog = join "\n" $
        maybe [] ((:: Nil) . show) prolog.xmlDecl ++
        map show prolog.xmlDeclMisc ++
        maybe [] ((:: Nil) . show) prolog.docType ++
        map show prolog.docTypeMisc

export
xmlProlog : Parser XMLProlog
xmlProlog = (do
    xmlDecl <- optional xmlDecl
    xmlDeclMisc <- many (spaces *> misc)
    docType <- optional (spaces *> docType)
    docTypeMisc <- many (spaces *> misc)
    pure $ MkXMLProlog xmlDecl xmlDeclMisc docType docTypeMisc
  ) <?> "XML prolog"
