module Language.XML.Element

import Data.List
import public Data.List.Alternating
import Data.String
import Data.String.Extra
import Data.String.Parser

import public Language.XML.Attribute

public export
data Element = EmptyElem String (List Attribute)
             | Elem String (List Attribute) (Odd (Maybe String) Element)

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
        "<\{name}\{concat $ map (\attr => " " ++ show attr) attrs}/>"
    show (Elem name attrs content) =
        """
        <\{name}\{concat $ map (\attr => " " ++ show attr) attrs}>\
        \{indentLines $ concat $ map ("\n" ++) $ catMaybes $ forget $ mapSnd (Just . show) content}\
        </\{name}>
        """

export
charData : Parser (Maybe String)
charData = do
    let word = map pack $ some $ satisfy $ \c => not (isSpace c) && c /= '<'
    words@(_ :: _) <- (many $ spaces *> word) <* spaces
        | [] => pure Nothing
    pure $ Just $ join " " words

export
element : Parser Element
element = (do
    ignore $ string "<"
    name <- pack <$> many letter
    attrs <- many (spaces *> attribute)
    spaces
    Nothing <- optional $ string "/>"
        | Just _ => pure $ EmptyElem name attrs
    ignore $ string ">"

    content <- alternating charData element

    string "</\{name}" *> spaces <* string ">"

    pure $ Elem name attrs content
  ) <?> "XML element"
