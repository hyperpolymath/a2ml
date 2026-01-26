// SPDX-License-Identifier: PMPL-1.0-or-later

let demoDoc = #""
# A2ML Overview

@abstract:
A2ML is a typed, attested markup format.
@end

## Claims
- Required sections must exist.
- References must resolve.
""#

let _ = {
  let parsed = A2ml.parse(demoDoc)
  let html = A2ml.renderHtml(parsed)
  let errors = A2ml.validate(parsed)
  // Replace with DOM updates in a real web integration.
  Js.log(html)
  Js.log(errors)
}
