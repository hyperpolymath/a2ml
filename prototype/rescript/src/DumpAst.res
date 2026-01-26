// SPDX-License-Identifier: PMPL-1.0-or-later

module Fs = {
  @module("fs")
  external readFileSync: (string, string) => string = "readFileSync"
}

let _ = {
  // Usage: node DumpAst.bs.js path/to/file.a2ml
  let args = Js.Sys.argv
  if Belt.Array.length(args) < 3 {
    Js.log("Usage: dump-ast <path>")
  } else {
    let path = args->Belt.Array.getExn(2)
    let input = Fs.readFileSync(path, "utf8")
    let doc = A2ml.parse(input)
    let json = Json.docToJson(doc)
    Js.log(Js.Json.stringifyAny(json))
  }
}
