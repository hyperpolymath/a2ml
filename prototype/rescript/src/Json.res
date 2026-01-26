// SPDX-License-Identifier: PMPL-1.0-or-later

let inlineToJson = (part: A2ml.inline): JSON.t => {
  switch part {
  | Text(t) =>
      JSON.Encode.object(Dict.fromArray([("type", JSON.Encode.string("text")), ("text", JSON.Encode.string(t))]))
  | Emph(t) =>
      JSON.Encode.object(Dict.fromArray([("type", JSON.Encode.string("emph")), ("text", JSON.Encode.string(t))]))
  | Strong(t) =>
      JSON.Encode.object(Dict.fromArray([("type", JSON.Encode.string("strong")), ("text", JSON.Encode.string(t))]))
  | Link(label, url) =>
      JSON.Encode.object(Dict.fromArray([
        ("type", JSON.Encode.string("link")),
        ("label", JSON.Encode.string(label)),
        ("url", JSON.Encode.string(url)),
      ]))
  }
}

let rec blockToJson = (block: A2ml.block): JSON.t => {
  switch block {
  | Heading(level, text) =>
      JSON.Encode.object(Dict.fromArray([
        ("type", JSON.Encode.string("heading")),
        ("level", JSON.Encode.float(Int.toFloat(level))),
        ("text", JSON.Encode.string(text)),
      ]))
  | Paragraph(parts) =>
      JSON.Encode.object(Dict.fromArray([
        ("type", JSON.Encode.string("paragraph")),
        ("inline", JSON.Encode.array(parts->Belt.Array.map(inlineToJson))),
      ]))
  | List(items) =>
      let rows = items->Belt.Array.map(parts => JSON.Encode.array(parts->Belt.Array.map(inlineToJson)))
      JSON.Encode.object(Dict.fromArray([
        ("type", JSON.Encode.string("list")),
        ("items", JSON.Encode.array(rows)),
      ]))
  | Directive(name, attrs, body) =>
      let attrsJson = attrs->Belt.Array.map(((k, v)) =>
        JSON.Encode.object(Dict.fromArray([("key", JSON.Encode.string(k)), ("value", JSON.Encode.string(v))]))
      )
      JSON.Encode.object(Dict.fromArray([
        ("type", JSON.Encode.string("directive")),
        ("name", JSON.Encode.string(name)),
        ("attrs", JSON.Encode.array(attrsJson)),
        ("body", JSON.Encode.array(body->Belt.Array.map(blockToJson))),
      ]))
  }
}

let docToJson = (doc: A2ml.doc): JSON.t => {
  JSON.Encode.array(doc->Belt.Array.map(blockToJson))
}
