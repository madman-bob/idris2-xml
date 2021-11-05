module Language.XML.Element

import Data.List
import public Data.List.Alternating
import Data.String
import Data.String.Parser

import public Language.XML.Attribute
import public Language.XML.CharData
import public Language.XML.Name

public export
data Element = EmptyElem QName (List Attribute)
             | Elem QName (List Attribute) (Odd CharData Element)

%name Element elem

public export
(.name) : Element -> QName
(EmptyElem name _).name = name
(Elem name _ _).name = name

public export
(.attrs) : Element -> List Attribute
(EmptyElem _ attrs).attrs = attrs
(Elem _ attrs _).attrs = attrs

public export
(.content) : Element -> Odd CharData Element
(EmptyElem _ _).content = [""]
(Elem _ _ content).content = content

public export
mapContent : (Odd CharData Element -> Odd CharData Element) -> Element -> Element
mapContent f (EmptyElem name attrs) = EmptyElem name attrs
mapContent f (Elem name attrs content) = Elem name attrs (f content)

indentLines : String -> String
indentLines str = unlines $ map indent $ lines str
  where
    indent : String -> String
    indent "" = ""
    indent s = "    " ++ s

export
Show Element where
    show (EmptyElem name attrs) =
        "<\{show name}\{concat $ map (\attr => " " ++ show attr) attrs}/>"
    show (Elem name attrs content) =
        """
        <\{show name}\{concat $ map (\attr => " " ++ show attr) attrs}>\
        \{indentLines $ concat $ map ("\n" ++) $ catMaybes $ forget $ assert_total $ bimap (.content) (Just . show) content}\
        </\{show name}>
        """

export
element : Parser Element
element = (do
    ignore $ string "<"
    name <- qName
    attrs <- many (spaces *> attribute)
    spaces
    Nothing <- optional $ string "/>"
        | Just _ => pure $ EmptyElem name attrs
    ignore $ string ">"

    content <- alternating charData element

    string "</\{show name}" *> spaces <* string ">"

    pure $ Elem name attrs content
  ) <?> "XML element"
