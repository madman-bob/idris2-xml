module Language.XML.Element

import Data.Either
import Data.List1
import public Data.List.Alternating
import Data.String
import Data.String.Extra
import Data.String.Parser

import public Language.XML.Attribute
import public Language.XML.CharData
import public Language.XML.Misc
import public Language.XML.Name

public export
data Element = EmptyElem QName (List Attribute)
             | Elem QName (List Attribute) (Odd CharData $ Either Misc Element)

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
(.content) : Element -> Odd CharData (Either Misc Element)
(EmptyElem _ _).content = [""]
(Elem _ _ content).content = content

public export
maybeNl : Bool -> String
maybeNl False = ""
maybeNl True = "\n"

public export
showNl : CharData -> String
showNl (MkCharData preSpace c postSpace) = maybeNl preSpace ++ c ++ maybeNl postSpace

public export
textContent : Element -> String
textContent (EmptyElem name attrs) = ""
textContent (Elem name attrs content) = concat $ Odd.forget $
    bimap showNl textContent $ content >>= either (const neutral) pure

public export
find : (Element -> Bool) -> Element -> Maybe Element
find f elem = find f (rights $ evens elem.content)

public export
mapContent : (Odd CharData (Either Misc Element) -> Odd CharData (Either Misc Element)) -> Element -> Element
mapContent f (EmptyElem name attrs) = EmptyElem name attrs
mapContent f (Elem name attrs content) = Elem name attrs (f content)

public export
mapContentM : Monad m => (Odd CharData (Either Misc Element) -> m (Odd CharData (Either Misc Element))) -> Element -> m Element
mapContentM f (EmptyElem name attrs) = pure $ EmptyElem name attrs
mapContentM f (Elem name attrs content) = pure $ Elem name attrs !(f content)

indentTail : String -> String
indentTail str =
    let (x ::: xs) = split (== '\n') str in
    join "\n" (x :: map indent xs)
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
        \{indentTail $ concat $ forget $ assert_total $ bimap showNl (either show show) content}\
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

    content <- alternating charData $ map Left misc <|> map Right element

    string "</\{show name}" *> spaces <* string ">"

    pure $ Elem name attrs content
  ) <?> "XML element"
