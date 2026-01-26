// SPDX-License-Identifier: PMPL-1.0-or-later

let _ = {
  let code = VectorRunner.run()
  if code != 0 {
    Js.Exn.raiseError("Vector tests failed")
  }
}
