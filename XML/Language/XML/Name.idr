module Language.XML.Name

import Data.String.Parser

public export
data Name = MkName String

%name Name name

export
Show Name where
    show (MkName n) = n

export
Eq Name where
    MkName n1 == MkName n2 = n1 == n2

public export
record QName where
    constructor MkQName
    namespacePrefix : Maybe Name
    localPart : Name

%name QName name

export
Show QName where
    show (MkQName Nothing localPart) = show localPart
    show (MkQName (Just namespacePrefix) localPart) = show namespacePrefix ++ ":" ++ show localPart

export
Eq QName where
    n1 == n2 = n1.namespacePrefix == n2.namespacePrefix && n1.localPart == n2.localPart

public export
isNameStartChar : Char -> Bool
isNameStartChar c = isAlpha c || c == '_'

public export
isNameChar : Char -> Bool
isNameChar c = isAlphaNum c || c == '.' || c == '-' || c == '_'

export
name : Parser Name
name = MkName <$> pack <$> [| satisfy isNameStartChar :: many (satisfy isNameChar) |]

export
qName : Parser QName
qName = do
    n <- name
    Just localPart <- optional (char ':' *> name)
        | Nothing => pure $ MkQName Nothing n
    pure $ MkQName (Just n) localPart
