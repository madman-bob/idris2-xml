module Language.XML.Prolog.XMLDecl

import Data.String.Parser

import Language.XML.Attribute

public export
record XMLDecl where
    constructor MkXMLDecl
    version : String
    encoding : Maybe String
    standalone : Maybe Bool

%name XMLDecl decl

export
Show XMLDecl where
    show decl = """
                <?xml version=\{show decl.version}\
                \{maybe "" (\e => " encoding=" ++ show e) decl.encoding}\
                \{maybe "" (\s => " standalone=" ++ if s then "\"yes\"" else "\"no\"") decl.standalone}\
                ?>
                """

export
xmlDecl : Parser XMLDecl
xmlDecl = (do
    ignore $ string "<?xml"
    spaces1
    version <- exactAttribute $ MkQName Nothing $ MkName "version"
    encoding <- optional (spaces *> exactAttribute (MkQName Nothing $ MkName "encoding"))
    standalone <- case !(optional (spaces *> exactAttribute (MkQName Nothing $ MkName "standalone"))) of
          Just "yes" => pure $ Just True
          Just "no" => pure $ Just False
          Nothing => pure Nothing
          Just value => fail "Expect \"yes\"/\"no\" for \"standalone\" attribute, got: \{show value}"
    spaces
    ignore $ string "?>"
    pure $ MkXMLDecl version encoding standalone
  ) <?> "XML declaration"
