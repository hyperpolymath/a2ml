// SPDX-License-Identifier: PMPL-1.0-or-later

// Minimal test vector runner for Module 0.

module Fs = {
  @module("fs")
  external readdirSync: string => array<string> = "readdirSync"

  @module("fs")
  external readFileSync: (string, string) => string = "readFileSync"

  @module("fs")
  external existsSync: string => bool = "existsSync"
}

let listVectors = (): array<(string, string)> => {
  let files = Fs.readdirSync("tests/vectors")
  files
  ->Belt.Array.keep(file => Js.String2.endsWith(file, ".a2ml"))
  ->Belt.Array.map(file => {
      let expected = Js.String2.replace(file, ".a2ml", ".expected")
      ("tests/vectors/" ++ file, "tests/vectors/" ++ expected)
    })
}

let parseExpected = (text: string): option<string> => {
  let lines = String.split(text, "\n")
  let errorLine = lines->Belt.Array.keep(line => Js.String2.startsWith(line, "ERROR:"))
  if Belt.Array.length(errorLine) > 0 {
    Some(String.trim(String.sliceToEnd(errorLine->Belt.Array.getExn(0), 6)))
  } else {
    None
  }
}

let normalizeHtml = (html: string): string => {
  // Collapse whitespace for stable comparison.
  let re = %re("\\s+")
  Js.String.replaceByRe(html, re, " ")->String.trim
}

let run = (): int => {
  let vectors = listVectors()
  let failures = Belt.Array.make(0, "")

  vectors->Belt.Array.forEach(((inputPath, expectedPath)) => {
    let input = Fs.readFileSync(inputPath, "utf8")
    let expected = Fs.readFileSync(expectedPath, "utf8")

    let doc = A2ml.parse(input)
    let errors = A2ml.validate(doc)
    let expectedError = parseExpected(expected)

    switch expectedError {
    | None =>
        if Belt.Array.length(errors) > 0 {
          failures->Belt.Array.push(inputPath ++ ": expected ok, got error")
        }
    | Some(msg) =>
        if Belt.Array.length(errors) == 0 {
          failures->Belt.Array.push(inputPath ++ ": expected error, got ok")
        } else {
          let first = errors->Belt.Array.getExn(0)
          if !Js.String2.includes(first.msg, msg) {
            failures->Belt.Array.push(inputPath ++ ": error mismatch")
          }
        }
    }

    let htmlExpectedPath = Js.String2.replace(inputPath, ".a2ml", ".html.expected")
    if Fs.existsSync(htmlExpectedPath) {
      let actualHtml = A2ml.renderHtml(doc)->normalizeHtml
      let expectedHtml = Fs.readFileSync(htmlExpectedPath, "utf8")->normalizeHtml
      if actualHtml != expectedHtml {
        failures->Belt.Array.push(inputPath ++ ": html mismatch")
      }
    }
  })

  if Belt.Array.length(failures) == 0 {
    Js.log("All vectors passed")
    0
  } else {
    failures->Belt.Array.forEach(msg => Js.log(msg))
    1
  }
}
