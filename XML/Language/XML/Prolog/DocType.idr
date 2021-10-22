module Language.XML.Prolog.DocType

import Data.String.Parser

import Language.XML.Attribute

public export
data ExternalID = System String | Public String String

%name ExternalID externalID

export
Show ExternalID where
    show (System sysID) = "SYSTEM \{show sysID}"
    show (Public pubID sysID) = "PUBLIC \{show pubID} \{show sysID}"

public export
record DocType where
    constructor MkDocType
    name : String
    externalID : Maybe ExternalID

export
Show DocType where
    show docType = """
                   <!DOCTYPE \{docType.name}\
                   \{maybe "" (\eID => " " ++ show eID) docType.externalID}\
                   >
                   """

export
systemID : Parser ExternalID
systemID = do
    ignore $ string "SYSTEM"
    spaces1
    System <$> quotedString

export
publicID : Parser ExternalID
publicID = do
    ignore $ string "PUBLIC"
    spaces1
    pubID <- quotedString
    spaces1
    sysID <- quotedString
    pure $ Public pubID sysID

export
externalID : Parser ExternalID
externalID = systemID <|> publicID

export
docType : Parser DocType
docType = (do
    ignore $ string "<!DOCTYPE"
    spaces1
    name <- pack <$> many letter
    externalID <- optional (spaces1 *> externalID)
    spaces
    ignore $ string ">"
    pure $ MkDocType name externalID
  ) <?> "XML document type"
