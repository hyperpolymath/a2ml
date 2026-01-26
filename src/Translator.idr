module A2ML.Translator

import A2ML.TypedCore
import A2ML.Surface

-- Minimal translation: headings become sections with generated IDs,
-- paragraphs and lists map directly, directives are ignored for now.

translate : SDoc -> Doc
translate (MkSDoc bs) = MkDoc (map toBlock bs)
  where
    mkId : String -> Id
    mkId t = MkId ("sec:" ++ t)

    inlineToText : List Inline -> String
    inlineToText = concatMap toString
      where
        toString : Inline -> String
        toString (SText s) = s
        toString (SEmph s) = s
        toString (SStrong s) = s
        toString (SLink label _) = label

    listToText : List (List Inline) -> List String
    listToText xs = map inlineToText xs

    toBlock : SBlock -> Block
    toBlock (SHeading _ t) = Section (MkSec (mkId t) t [])
    toBlock (SParagraph t) = Para (inlineToText t)
    toBlock (SList xs) = Bullet (listToText xs)
    toBlock (SDirective _ _ body) =
      -- For now, discard directive wrapper and translate its body
      Section (MkSec (mkId "directive") "Directive" (map toBlock body))
