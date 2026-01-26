// SPDX-License-Identifier: PMPL-1.0-or-later

let hasDeno = %raw(`typeof Deno !== "undefined"`)

let readText = (path: string): string => {
  %raw(`(typeof Deno !== "undefined")
    ? Deno.readTextFileSync(path)
    : require("fs").readFileSync(path, "utf8")`)
}

let writeText = (path: string, text: string): unit => {
  %raw(`(typeof Deno !== "undefined")
    ? Deno.writeTextFileSync(path, text)
    : require("fs").writeFileSync(path, text, "utf8")`)
}

let exit = (code: int): unit => {
  %raw(`(typeof Deno !== "undefined") ? Deno.exit(code) : process.exit(code)`)
}

let usage = () => {
  Js.log("a2ml <render|validate|ast> <file> [--out path] [--mode lax|checked]")
  exit(1)
}

let getArg = (args: array<string>, name: string): option<string> => {
  let rec loop = i =>
    if i >= Belt.Array.length(args) { None }
    else if Belt.Array.getExn(args, i) == name {
      if i + 1 < Belt.Array.length(args) { Some(Belt.Array.getExn(args, i + 1)) } else None
    } else {
      loop(i + 1)
    }
  loop(0)
}

let _ = {
  let args = Js.Sys.argv
  if Belt.Array.length(args) < 3 { usage() }

  let command = Belt.Array.getExn(args, 2)
  let file = if Belt.Array.length(args) >= 4 { Belt.Array.getExn(args, 3) } else { usage(); "" }

  let mode = switch getArg(args, "--mode") {
  | Some("checked") => A2ml.Checked
  | _ => A2ml.Lax
  }

  let outPath = getArg(args, "--out")

  let input = readText(file)
  let doc = A2ml.parse(~mode, input)

  switch command {
  | "render" =>
      let html = A2ml.renderHtml(doc)
      switch outPath {
      | Some(p) => writeText(p, html)
      | None => Js.log(html)
      }
  | "validate" =>
      let errs = A2ml.validateChecked(doc)
      if Belt.Array.length(errs) == 0 {
        Js.log("ok")
      } else {
        errs->Belt.Array.forEach(e => Js.Console.error(`${Int.toString(e.line)}: ${e.msg}`))
        exit(2)
      }
  | "ast" =>
      let json = Json.docToJson(doc)
      let text = Js.Json.stringifyAny(json)
      switch outPath {
      | Some(p) => writeText(p, text)
      | None => Js.log(text)
      }
  | _ => usage()
  }
}
