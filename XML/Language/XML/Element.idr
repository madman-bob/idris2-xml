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
        \{indentLines $ concat $ map ("\n" ++) $ catMaybes $ forget $ bimap (.content) (Just . show) content}\
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
