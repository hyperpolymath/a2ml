module A2ML.TypedCore

%default total

-- Minimal typed core stub for v0.2 development.

public export
record Doc where
  constructor MkDoc
  blocks : List Block

public export
data Block
  = Section Sec
  | Para String
  | Bullet (List String)
  | Figure Fig
  | Table Tbl
  | Refs (List Ref)
  | Opaque Payload

public export
record Sec where
  constructor MkSec
  id : String
  title : String
  body : List Block

public export
record Fig where
  constructor MkFig
  id : String
  caption : String
  ref : Maybe String

public export
record Tbl where
  constructor MkTbl
  id : String
  caption : String

public export
record Ref where
  constructor MkRef
  label : String

public export
record Payload where
  constructor MkPayload
  id : Maybe String
  lang : Maybe String
  bytes : String

-- Placeholder invariants (proofs to be defined in v0.3)
public export
UniqueIds : Doc -> Type
UniqueIds _ = Unit

public export
RefsResolve : Doc -> Type
RefsResolve _ = Unit

public export
HasAbstract : Doc -> Type
HasAbstract _ = Unit
