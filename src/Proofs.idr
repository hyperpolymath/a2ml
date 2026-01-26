module A2ML.Proofs

import A2ML.TypedCore

%default total

-- Decidable proofs for v1 predicates

uniqueIdsDec : (doc : Doc) -> Dec (UniqueIds doc)
uniqueIdsDec doc = decEq (uniqueIdsB doc) True

refsResolveDec : (doc : Doc) -> Dec (RefsResolve doc)
refsResolveDec doc = decEq (refsResolveB doc) True

hasAbstractDec : (doc : Doc) -> Dec (HasAbstract doc)
hasAbstractDec doc = decEq (hasAbstractB doc) True
