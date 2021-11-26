module Language.XML.CharData

import public Data.List
import public Data.String
import Data.String.Extra
import Data.String.Parser

public export
record CharData where
    constructor MkCharData
    preSpace : Bool
    c : String
    postSpace : Bool

%name CharData c, d

public export
maybeSpace : Bool -> String
maybeSpace False = ""
maybeSpace True = " "

export
Show CharData where
    show (MkCharData preSpace c postSpace) = maybeSpace preSpace ++ c ++ maybeSpace postSpace

public export
fromString : String -> CharData
fromString str = case unpack str of
    [] => MkCharData False "" False
    cs@(_ :: _) =>
        let s = trim str in
        MkCharData (isSpace $ head cs) s ((not $ null s) && (isSpace $ last cs))

public export
(++) : CharData -> CharData -> CharData
(MkCharData preSpace "" midLeftSpace) ++ (MkCharData midRightSpace d postSpace) =
    MkCharData (preSpace || midLeftSpace || midRightSpace) d postSpace
(MkCharData preSpace c midLeftSpace) ++ (MkCharData midRightSpace "" postSpace) =
    MkCharData preSpace c (midLeftSpace || midRightSpace || postSpace)
(MkCharData preSpace c midLeftSpace) ++ (MkCharData midRightSpace d postSpace) =
    MkCharData preSpace (c ++ maybeSpace (midLeftSpace || midRightSpace) ++ d) postSpace

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

    preSpace <- succeeds spaces1
    words <- (many $ spaces *> word)
    postSpace <- succeeds spaces1

    pure $ MkCharData preSpace (join " " words) postSpace
