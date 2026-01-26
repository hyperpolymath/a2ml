// SPDX-License-Identifier: PMPL-1.0-or-later

// Minimal Module-0 parser and checked validator for web rendering demos.
// This is intentionally small but deterministic.

type attrs = array<(string, string)>

type block =
  | Heading(int, string)
  | Paragraph(string)
  | List(array<string>)
  | Directive(string, attrs, array<string>)

type doc = array<block>

type parseMode =
  | Lax
  | Checked

type parseError = {
  line: int,
  msg: string,
}

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

let parseAttrs = (line: string): attrs => {
  // Parse "@name(a=b,c=d):" into [("a","b"),("c","d")]
  let start = switch String.indexOf(line, "(") {
  | None => -1
  | Some(idx) => idx
  }
  let end_ = switch String.indexOf(line, ")") {
  | None => -1
  | Some(idx) => idx
  }
  if start == -1 || end_ == -1 || end_ < start {
    [||]
  } else {
    let inner = String.slice(line, start + 1, end_)
    let parts = String.split(inner, ",")
    parts
    ->Belt.Array.keepMap(part => {
        let kv = String.split(String.trim(part), "=")
        if Belt.Array.length(kv) == 2 {
          let key = kv->Belt.Array.getExn(0)->String.trim
          let value = kv->Belt.Array.getExn(1)->String.trim
          Some((key, value))
        } else {
          None
        }
      })
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
            let header = String.trim(line)
            let name = String.sliceToEnd(header, 1)
            let nameOnly = switch String.indexOf(name, ":") {
              | None => name
              | Some(idx) => String.slice(name, 0, idx)
            }
            let attrs = parseAttrs(header)
            let rec collect = (j, acc) =>
              if j >= Belt.Array.length(lines) { (j, acc) } else {
                let l = Belt.Array.getExn(lines, j)
                if String.trim(l) == "@end" { (j + 1, acc) }
                else { collect(j + 1, Belt.Array.concat(acc, [|l|])) }
              }
            let (nextIndex, body) = collect(i + 1, [||])
            blocks->Belt.Array.push(Directive(nameOnly, attrs, body))
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
      | Directive(name, _attrs, body) =>
          let content = body->Belt.Array.joinWith("\n")
          "<div data-a2ml=\"" ++ name ++ "\">" ++ content ++ "</div>"
      }
    )
  ->Belt.Array.joinWith("\n")
}

let validate = (doc: doc): array<parseError> => {
  let ids = Belt.Set.String.make()
  let refs = Belt.Array.make(0, ("", 0))
  let errors = Belt.Array.make(0, {line: 0, msg: ""})

  doc->Belt.Array.forEachWithIndex((i, block) => {
    switch block {
    | Directive(_name, attrs, _body) =>
        attrs->Belt.Array.forEach(((k, v)) => {
          if k == "id" {
            if Belt.Set.String.has(ids, v) {
              errors->Belt.Array.push({line: i + 1, msg: "duplicate id: " ++ v})
            } else {
              Belt.Set.String.add(ids, v)->ignore
            }
          } else if k == "ref" {
            refs->Belt.Array.push((v, i + 1))
          } else {
            ()
          }
        })
    | _ => ()
    }
  })

  refs->Belt.Array.forEach(((refId, lineNo)) => {
    if !Belt.Set.String.has(ids, refId) {
      errors->Belt.Array.push({line: lineNo, msg: "unresolved reference: " ++ refId})
    }
  })

  errors
}
