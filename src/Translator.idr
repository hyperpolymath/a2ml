module A2ML.Translator

import A2ML.TypedCore

-- Minimal surface AST for translation stub
public export
data SBlock
  = SHeading Int String
  | SParagraph String
  | SList (List String)

public export
record SDoc where
  constructor MkSDoc
  blocks : List SBlock

-- Stub translation: headings become sections with generated IDs
translate : SDoc -> Doc
translate (MkSDoc bs) = MkDoc (map toBlock bs)
  where
    mkId : String -> Id
    mkId t = MkId ("sec:" ++ t)

    toBlock : SBlock -> Block
    toBlock (SHeading _ t) = Section (MkSec (mkId t) t [])
    toBlock (SParagraph t) = Para t
    toBlock (SList xs) = Bullet xs
