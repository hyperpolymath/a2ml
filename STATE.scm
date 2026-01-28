;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state for a2ml

(state
  (metadata
    (version "0.1.0")  ; Only v0.1.0 is tagged; v0.6.0 features exist but not released
    (schema-version "1.0")
    (created "2026-01-26")
    (updated "2026-01-28")
    (project "a2ml")
    (repo "hyperpolymath/a2ml"))

  (project-context
    (name "A2ML")
    (tagline "Attested Markup Language")
    (tech-stack ("spec" "rescript" "idris2")))

  (current-position
    (phase "prototype")
    (overall-completion 40)  ; Increased from 35 - working code exists
    (working-features
      ("Surface grammar implemented"
       "ReScript parser and renderer functional"
       "CLI tools (validate, render, ast)"
       "23 test vectors defined"
       "Typed core outline (Idris2)"
       "Module 0 quickstart"))
    (needs-work
      ("Test runner (justfile uses incompatible ReScript version)"
       "Ada TUI incomplete"
       "Git tags for v0.5.0, v0.6.0 never created"))))
