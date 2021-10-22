module Language.XML.Misc

import Data.String.Parser

public export
data Misc = Comment String | ProcessingInstruction String String

%name Misc misc

export
Show Misc where
    show (Comment comment) = "<!--\{comment}-->"
    show (ProcessingInstruction target instruction) = "<?\{target} \{instruction}?>"

export
comment : Parser Misc
comment = (map Comment (string "<!--" *> takeUntil "-->")) <?> "XML comment"

export
processingInstruction : Parser Misc
processingInstruction = (do
    ignore $ string "<?"
    target <- pack <$> many letter
    spaces1
    instruction <- takeUntil "?>"
    pure $ ProcessingInstruction target instruction
  ) <?> "XML processing instruction"

export
misc : Parser Misc
misc = comment <|> processingInstruction
