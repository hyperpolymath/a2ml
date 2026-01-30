module A2ML.Proofs

import A2ML.TypedCore
import Decidable.Equality

%default total

-- Decidable proofs for v1 predicates

export covering  -- Depends on partial functions in TypedCore
uniqueIdsDec : (doc : Doc) -> Dec (UniqueIds doc)
uniqueIdsDec doc = case decEq (uniqueIdsB doc) True of
  Yes prf => Yes prf
  No contra => No contra

export covering
refsResolveDec : (doc : Doc) -> Dec (RefsResolve doc)
refsResolveDec doc = case decEq (refsResolveB doc) True of
  Yes prf => Yes prf
  No contra => No contra

export covering
hasAbstractDec : (doc : Doc) -> Dec (HasAbstract doc)
hasAbstractDec doc = case decEq (hasAbstractB doc) True of
  Yes prf => Yes prf
  No contra => No contra
