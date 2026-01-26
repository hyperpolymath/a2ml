;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for a2ml
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "a2ml")
  (type "spec")
  (purpose "Typed, attested markup language")

  (position-in-ecosystem
    (category "documentation")
    (subcategory "markup")
    (unique-value
      ("Lightweight surface syntax with typed attestation")))

  (related-projects
    ("stateful-artefacts-for-gitforges"
     "k9-svc"))

  (what-this-is
    ("A markup format with a typed core"
     "A surface syntax designed for gradual strictness"
     "A spec-first effort"))

  (what-this-is-not
    ("A semantic web ontology"
     "A runtime execution format")))
