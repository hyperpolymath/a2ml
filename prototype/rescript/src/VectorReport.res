// SPDX-License-Identifier: PMPL-1.0-or-later

module Fs = {
  @module("fs")
  external readdirSync: string => array<string> = "readdirSync"

  @module("fs")
  external readFileSync: (string, string) => string = "readFileSync"

  @module("fs")
  external existsSync: string => bool = "existsSync"

  @module("fs")
  external writeFileSync: (string, string) => unit = "writeFileSync"
}

let listVectors = (): array<(string, string)> => {
let files = Fs.readdirSync("tests/vectors")
files
->Belt.Array.keep(file => String.endsWith(file, ".a2ml"))
->Belt.Array.map(file => {
    let expected = String.replace(file, ".a2ml", ".expected")
    ("tests/vectors/" ++ file, "tests/vectors/" ++ expected)
  })
}

let parseExpected = (text: string): option<string> => {
  let lines = String.split(text, "\n")
  let errorLine = lines->Belt.Array.keep(line => String.startsWith(line, "ERROR:"))
  if Belt.Array.length(errorLine) > 0 {
    Some(String.trim(String.slice(errorLine->Belt.Array.getExn(0), ~start=6)))
  } else {
    None
  }
}

let normalizeHtml = (html: string): string => {
  let re = %re("/\\s+/")
  Js.String.replaceByRe(re, " ", html)->String.trim
}

type report = {
  file: string,
  ok: bool,
  errors: array<string>,
}

let run = (): array<report> => {
  let vectors = listVectors()
  vectors
  ->Belt.Array.map(((inputPath, expectedPath)) => {
      let input = Fs.readFileSync(inputPath, "utf8")
      let expected = Fs.readFileSync(expectedPath, "utf8")

      let doc = A2ml.parse(input)
      let errors = A2ml.validateChecked(doc)
      let expectedError = parseExpected(expected)
      let msgs = Belt.Array.make(0, "")

      switch expectedError {
      | None =>
          if Belt.Array.length(errors) > 0 {
            msgs->Belt.Array.push("expected ok, got error")
          }
      | Some(msg) =>
          if Belt.Array.length(errors) == 0 {
            msgs->Belt.Array.push("expected error, got ok")
          } else {
            let first = errors->Belt.Array.getExn(0)
  if !String.includes(first.msg, msg) {
              msgs->Belt.Array.push("error mismatch")
            }
          }
      }

let htmlExpectedPath = String.replace(inputPath, ".a2ml", ".html.expected")
      if Fs.existsSync(htmlExpectedPath) {
        let actualHtml = A2ml.renderHtml(doc)->normalizeHtml
        let expectedHtml = Fs.readFileSync(htmlExpectedPath, "utf8")->normalizeHtml
        if actualHtml != expectedHtml {
          msgs->Belt.Array.push("html mismatch")
        }
      }

      {file: inputPath, ok: Belt.Array.length(msgs) == 0, errors: msgs}
    })
}

let _ = {
  let results = run()
  let json =
    switch JSON.stringifyAny(results) {
    | Some(value) => value
    | None => "[]"
    }
  Fs.writeFileSync("build/vector-report.json", json)
  let failures = results->Belt.Array.keep(r => !r.ok)
  if Belt.Array.length(failures) == 0 {
  Console.log("All vectors passed")
  } else {
  failures
  ->Belt.Array.forEach(r =>
      Console.error(`${r.file}: ${r.errors->Belt.Array.joinWith(", ", s => s)}`)
    )
    %raw(`(typeof Deno !== "undefined") ? Deno.exit(2) : process.exit(2)`)
  }
}
