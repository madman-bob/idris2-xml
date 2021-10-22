module Language.XML.Attribute

import Data.String.Parser

public export
record Attribute where
    constructor MkAttribute
    name : String
    value : String

%name Attribute attr

export
Show Attribute where
    show attr = "\{attr.name}=\{show attr.value}"

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
    name <- pack <$> many letter
    spaces
    ignore $ string "="
    spaces
    value <- quotedString
    pure $ MkAttribute name value
  ) <?> "XML attribute"

export
exactAttribute : String -> Parser String
exactAttribute expectedName = do
    MkAttribute name value <- attribute
    if name == expectedName
        then pure value
        else fail "Unexpected attribute: \{show name}"
