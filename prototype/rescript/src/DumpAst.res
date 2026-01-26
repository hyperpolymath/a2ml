// SPDX-License-Identifier: PMPL-1.0-or-later

module Fs = {
  @module("fs")
  external readFileSync: (string, string) => string = "readFileSync"
}

@val external argv: array<string> = "process.argv"

let _ = {
  // Usage: node DumpAst.bs.js path/to/file.a2ml
  let args = argv
  if Belt.Array.length(args) < 3 {
    Console.log("Usage: dump-ast <path>")
  } else {
    let path = args->Belt.Array.getExn(2)
    let input = Fs.readFileSync(path, "utf8")
    let doc = A2ml.parse(input)
    let json = Json.docToJson(doc)
    let out =
      switch JSON.stringifyAny(json) {
      | Some(value) => value
      | None => "{}"
      }
    Console.log(out)
  }
}
