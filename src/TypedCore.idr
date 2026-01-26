module A2ML.TypedCore

%default total

-- Minimal typed core stub for v0.2 development.

public export
record Id where
  constructor MkId
  raw : String

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
  id : Id
  title : String
  body : List Block

public export
record Fig where
  constructor MkFig
  id : Id
  caption : String
  ref : Maybe Id

public export
record Tbl where
  constructor MkTbl
  id : Id
  caption : String

public export
record Ref where
  constructor MkRef
  label : String

public export
data RefTarget
  = RefSection Id
  | RefFigure Id
  | RefTable Id

public export
record Payload where
  constructor MkPayload
  id : Maybe Id
  lang : Maybe String
  bytes : String

-- Executable checks (v0.2)

collectIds : Doc -> List Id
collectIds (MkDoc blocks) = concatMap collectBlock blocks
  where
    collectBlock : Block -> List Id
    collectBlock (Section s) = s.id :: collectIds (MkDoc s.body)
    collectBlock (Figure f) = [f.id]
    collectBlock (Table t) = [t.id]
    collectBlock (Opaque p) = maybe [] (id => [rid]) p.id
    collectBlock _ = []

collectRefs : Doc -> List Id
collectRefs (MkDoc blocks) = concatMap collectBlock blocks
  where
    collectBlock : Block -> List Id
    collectBlock (Section s) = collectRefs (MkDoc s.body)
    collectBlock (Figure f) = maybe [] (id => [rid]) f.ref
    collectBlock _ = []

idEq : Id -> Id -> Bool
idEq (MkId a) (MkId b) = a == b

contains : Id -> List Id -> Bool
contains _ [] = False
contains x (y :: ys) = if idEq x y then True else contains x ys

hasDuplicate : List Id -> Bool
hasDuplicate [] = False
hasDuplicate (x :: xs) = if contains x xs then True else hasDuplicate xs

allIn : List Id -> List Id -> Bool
allIn [] _ = True
allIn (x :: xs) ys = contains x ys && allIn xs ys

uniqueIdsB : Doc -> Bool
uniqueIdsB doc = not (hasDuplicate (collectIds doc))

refsResolveB : Doc -> Bool
refsResolveB doc = allIn (collectRefs doc) (collectIds doc)

hasAbstractB : Doc -> Bool
hasAbstractB (MkDoc blocks) = any isAbstract blocks
  where
    isAbstract : Block -> Bool
    isAbstract (Section s) = s.title == "Abstract"
    isAbstract _ = False

validateDoc : Doc -> List String
validateDoc doc =
  let errs1 = if uniqueIdsB doc then [] else ["duplicate ids"]
      errs2 = if refsResolveB doc then [] else ["unresolved references"]
  in errs1 ++ errs2

-- Placeholder proof types (v0.3+)
public export
UniqueIds : Doc -> Type
UniqueIds _ = Unit

public export
RefsResolve : Doc -> Type
RefsResolve _ = Unit

public export
HasAbstract : Doc -> Type
HasAbstract _ = Unit
