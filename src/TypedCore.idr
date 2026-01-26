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

-- Executable checks (v0.2)

collectIds : Doc -> List String
collectIds (MkDoc blocks) = concatMap collectBlock blocks
  where
    collectBlock : Block -> List String
    collectBlock (Section s) = s.id :: collectIds (MkDoc s.body)
    collectBlock (Figure f) = [f.id]
    collectBlock (Table t) = [t.id]
    collectBlock (Opaque p) = maybe [] (\x => [x]) p.id
    collectBlock _ = []

collectRefs : Doc -> List String
collectRefs (MkDoc blocks) = concatMap collectBlock blocks
  where
    collectBlock : Block -> List String
    collectBlock (Section s) = collectRefs (MkDoc s.body)
    collectBlock (Figure f) = maybe [] (\x => [x]) f.ref
    collectBlock _ = []

contains : String -> List String -> Bool
contains _ [] = False
contains x (y :: ys) = if x == y then True else contains x ys

hasDuplicate : List String -> Bool
hasDuplicate [] = False
hasDuplicate (x :: xs) = if contains x xs then True else hasDuplicate xs

allIn : List String -> List String -> Bool
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
