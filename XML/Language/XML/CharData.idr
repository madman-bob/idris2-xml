module Language.XML.CharData

import Data.String.Extra
import Data.String.Parser

public export
data CharData = MkCharData String

%name CharData c, d

public export
(.content) : CharData -> Maybe String
(MkCharData "").content = Nothing
(MkCharData c).content = Just c

export
Show CharData where
    show (MkCharData c) = c

public export
fromString : String -> CharData
fromString = MkCharData

public export
(++) : CharData -> CharData -> CharData
"" ++ d = d
c ++ "" = c
(MkCharData c) ++ (MkCharData d) = MkCharData $ c ++ " " ++ d

public export
Semigroup CharData where
    (<+>) = (++)

public export
Monoid CharData where
    neutral = ""

export
charData : Parser CharData
charData = do
    let word = map pack $ some $ satisfy $ \c => not (isSpace c) && c /= '<'
    words@(_ :: _) <- (many $ spaces *> word) <* spaces
        | [] => pure ""
    pure $ MkCharData $ join " " words
