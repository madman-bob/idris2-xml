module Language.XML.Attribute

import Data.List
import Data.String.Parser

import public Language.XML.Name

public export
record Attribute where
    constructor MkAttribute
    name : QName
    value : String

%name Attribute attr

public export
lookup : (name : QName) -> List Attribute -> Maybe String
lookup name attrs = lookup name $ map (\attr => (attr.name, attr.value)) attrs

export
Show Attribute where
    show attr = "\{show attr.name}=\{show attr.value}"

export
quotedString : Parser String
quotedString = do
    quote <- satisfy (\c => c == '"' || c == '\'')
    content <- map pack $ many $ satisfy (/= quote)
    ignore $ char quote
    pure content

export
attribute : Parser Attribute
attribute = (do
    name <- qName
    spaces
    ignore $ string "="
    spaces
    value <- quotedString
    pure $ MkAttribute name value
  ) <?> "XML attribute"

export
exactAttribute : QName -> Parser String
exactAttribute expectedName = do
    MkAttribute name value <- attribute
    if name == expectedName
        then pure value
        else fail "Unexpected attribute: \{show name}"
