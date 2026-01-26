// SPDX-License-Identifier: PMPL-1.0-or-later

let inlineToJson = (part: A2ml.inline): Js.Json.t => {
  switch part {
  | Text(t) => Js.Json.object_(Js.Dict.fromArray([|("type", Js.Json.string("text")), ("text", Js.Json.string(t))|]))
  | Emph(t) => Js.Json.object_(Js.Dict.fromArray([|("type", Js.Json.string("emph")), ("text", Js.Json.string(t))|]))
  | Strong(t) => Js.Json.object_(Js.Dict.fromArray([|("type", Js.Json.string("strong")), ("text", Js.Json.string(t))|]))
  | Link(label, url) =>
      Js.Json.object_(Js.Dict.fromArray([|
        ("type", Js.Json.string("link")),
        ("label", Js.Json.string(label)),
        ("url", Js.Json.string(url)),
      |]))
  }
}

let rec blockToJson = (block: A2ml.block): Js.Json.t => {
  switch block {
  | Heading(level, text) =>
      Js.Json.object_(Js.Dict.fromArray([|
        ("type", Js.Json.string("heading")),
        ("level", Js.Json.number(float_of_int(level))),
        ("text", Js.Json.string(text)),
      |]))
  | Paragraph(parts) =>
      Js.Json.object_(Js.Dict.fromArray([|
        ("type", Js.Json.string("paragraph")),
        ("inline", Js.Json.array(parts->Belt.Array.map(inlineToJson))),
      |]))
  | List(items) =>
      let rows = items->Belt.Array.map(parts => Js.Json.array(parts->Belt.Array.map(inlineToJson)))
      Js.Json.object_(Js.Dict.fromArray([|
        ("type", Js.Json.string("list")),
        ("items", Js.Json.array(rows)),
      |]))
  | Directive(name, attrs, body) =>
      let attrsJson = attrs->Belt.Array.map(((k, v)) =>
        Js.Json.object_(Js.Dict.fromArray([|("key", Js.Json.string(k)), ("value", Js.Json.string(v))|]))
      )
      Js.Json.object_(Js.Dict.fromArray([|
        ("type", Js.Json.string("directive")),
        ("name", Js.Json.string(name)),
        ("attrs", Js.Json.array(attrsJson)),
        ("body", Js.Json.array(body->Belt.Array.map(blockToJson))),
      |]))
  }
}

let docToJson = (doc: A2ml.doc): Js.Json.t => {
  Js.Json.array(doc->Belt.Array.map(blockToJson))
}
