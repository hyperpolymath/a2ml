// SPDX-License-Identifier: PMPL-1.0-or-later

// Minimal Module-0 parser sketch for web rendering demos.
// This is intentionally small and non-exhaustive.

type block =
  | Heading(int, string)
  | Paragraph(string)
  | List(array<string>)
  | Directive(string, array<string>)

type doc = array<block>

type parseMode =
  | Lax
  | Checked

let isHeading = (line: string): option<(int, string)> => {
  let trimmed = String.trim(line)
  let rec countHashes = (i, count) =>
    if i >= String.length(trimmed) {count} else {
      if String.get(trimmed, i) == '#' {countHashes(i + 1, count + 1)} else {count}
    }
  let hcount = countHashes(0, 0)
  if hcount > 0 && hcount <= 5 {
    let text = String.trim(String.sliceToEnd(trimmed, hcount))
    Some((hcount, text))
  } else {
    None
  }
}

let parse = (~mode: parseMode=Lax, input: string): doc => {
  let lines = String.split(input, "\n")
  let blocks = Belt.Array.make(0, Paragraph(""))

  let rec loop = i => {
    if i >= Belt.Array.length(lines) {
      ()
    } else {
      let line = Belt.Array.getExn(lines, i)
      if String.trim(line) == "" {
        loop(i + 1)
      } else {
        switch isHeading(line) {
        | Some((level, text)) => {
            blocks->Belt.Array.push(Heading(level, text))
            loop(i + 1)
          }
        | None =>
          if String.startsWith(String.trim(line), "@") {
            let name = String.sliceToEnd(String.trim(line), 1)
            let nameOnly = switch String.indexOf(name, ":") {
              | None => name
              | Some(idx) => String.slice(name, 0, idx)
            }
            let rec collect = (j, acc) =>
              if j >= Belt.Array.length(lines) { (j, acc) } else {
                let l = Belt.Array.getExn(lines, j)
                if String.trim(l) == "@end" { (j + 1, acc) }
                else { collect(j + 1, Belt.Array.concat(acc, [|l|])) }
              }
            let (nextIndex, body) = collect(i + 1, [||])
            blocks->Belt.Array.push(Directive(nameOnly, body))
            loop(nextIndex)
          } else if String.startsWith(String.trim(line), "-") {
            let rec collect = (j, acc) =>
              if j >= Belt.Array.length(lines) { (j, acc) } else {
                let l = String.trim(Belt.Array.getExn(lines, j))
                if String.startsWith(l, "-") {
                  let item = String.trim(String.sliceToEnd(l, 1))
                  collect(j + 1, Belt.Array.concat(acc, [|item|]))
                } else {
                  (j, acc)
                }
              }
            let (nextIndex, items) = collect(i, [||])
            blocks->Belt.Array.push(List(items))
            loop(nextIndex)
          } else {
            blocks->Belt.Array.push(Paragraph(String.trim(line)))
            loop(i + 1)
          }
        }
      }
    }
  }

  loop(0)
  blocks
}

let renderHtml = (doc: doc): string => {
  doc
  ->Belt.Array.map(block =>
      switch block {
      | Heading(level, text) =>
          "<h" ++ string_of_int(level) ++ ">" ++ text ++ "</h" ++ string_of_int(level) ++ ">"
      | Paragraph(text) => "<p>" ++ text ++ "</p>"
      | List(items) =>
          let lis = items->Belt.Array.map(item => "<li>" ++ item ++ "</li>")->Belt.Array.joinWith("")
          "<ul>" ++ lis ++ "</ul>"
      | Directive(name, body) =>
          let content = body->Belt.Array.joinWith("\n")
          "<div data-a2ml=\"" ++ name ++ "\">" ++ content ++ "</div>"
      }
    )
  ->Belt.Array.joinWith("\n")
}
