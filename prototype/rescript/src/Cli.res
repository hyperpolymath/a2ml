// SPDX-License-Identifier: PMPL-1.0-or-later

let hasDeno = %raw(`typeof Deno !== "undefined"`)

@val external argv: array<string> = "process.argv"

let readText = (path: string): string => {
  let _ = path
  %raw(`(typeof Deno !== "undefined")
    ? Deno.readTextFileSync(path)
    : require("fs").readFileSync(path, "utf8")`)
}

let writeText = (path: string, text: string): unit => {
  let _ = path
  let _ = text
  %raw(`(typeof Deno !== "undefined")
    ? Deno.writeTextFileSync(path, text)
    : require("fs").writeFileSync(path, text, "utf8")`)
}

let readStdin = (): string => {
  %raw(`(typeof Deno !== "undefined")
    ? new TextDecoder().decode(Deno.readAllSync(Deno.stdin))
    : require("fs").readFileSync(0, "utf8")`)
}

let exit = (code: int): unit => {
  let _ = code
  %raw(`(typeof Deno !== "undefined") ? Deno.exit(code) : process.exit(code)`)
}

let helpText = "A2ML CLI (prototype)\n\n" ++
  "Usage:\n  a2ml <render|validate|ast> <file...|-> [options]\n\n" ++
  "Commands:\n  render    Render HTML to stdout (or --out)\n" ++
  "  validate  Validate in checked mode, exit 2 on errors\n" ++
  "  ast       Output JSON surface AST\n\n" ++
  "Options:\n  --mode <lax|checked>   Parse mode (default: lax)\n" ++
  "  --out <path>           Write output to file\n" ++
  "  --concat               Concatenate outputs when multiple inputs\n" ++
  "  --stdin                Read input from stdin (equivalent to '-')\n" ++
  "  -h, --help             Show this help\n\n" ++
  "Notes:\n  * Use '-' as a filename to read from stdin.\n" ++
  "  * For multiple files, --concat joins outputs in order.\n"

let usage = (): unit => {
  Console.log(helpText)
  exit(1)
}

let getArg = (args: array<string>, name: string): option<string> => {
  let rec loop = i =>
    if i >= Belt.Array.length(args) {
      None
    } else if Belt.Array.getExn(args, i) == name {
      if i + 1 < Belt.Array.length(args) {
        Some(Belt.Array.getExn(args, i + 1))
      } else {
        None
      }
    } else {
      loop(i + 1)
    }
  loop(0)
}

let hasFlag = (args: array<string>, name: string): bool => {
  Belt.Array.some(args, arg => arg == name)
}

let collectInputs = (args: array<string>): array<string> => {
  let inputs = Belt.Array.make(0, "")
  let rec loop = i =>
    if i >= Belt.Array.length(args) {
      inputs
    } else {
      let arg = Belt.Array.getExn(args, i)
      if String.startsWith(arg, "-") {
        inputs
      } else {
        inputs->Belt.Array.push(arg)
        loop(i + 1)
      }
    }
  loop(3)
}

let readInput = (path: string): string => {
  if path == "-" { readStdin() } else { readText(path) }
}

let renderDoc = (input: string, mode: A2ml.parseMode): string => {
  let doc = A2ml.parse(~mode, input)
  A2ml.renderHtml(doc)
}

let validateDoc = (input: string, mode: A2ml.parseMode): array<A2ml.parseError> => {
  let doc = A2ml.parse(~mode, input)
  if mode == A2ml.Checked { A2ml.validateChecked(doc) } else { [] }
}

let astDoc = (input: string, mode: A2ml.parseMode): string => {
  let doc = A2ml.parse(~mode, input)
  switch JSON.stringifyAny(Json.docToJson(doc)) {
  | Some(value) => value
  | None => "{}"
  }
}

let _ = {
  let args = argv
  if hasFlag(args, "-h") || hasFlag(args, "--help") { Console.log(helpText); exit(0) }
  if Belt.Array.length(args) < 3 { usage() }

  let command = Belt.Array.getExn(args, 2)
  let inputs = collectInputs(args)
  let readFromStdin = hasFlag(args, "--stdin")

  let mode = switch getArg(args, "--mode") {
  | Some("checked") => A2ml.Checked
  | _ => A2ml.Lax
  }

  let outPath = getArg(args, "--out")
  let concat = hasFlag(args, "--concat")

  let sources = if readFromStdin { ["-"] } else { inputs }
  if Belt.Array.length(sources) == 0 { usage() }

  switch command {
  | "render" =>
      let outputs = sources->Belt.Array.map(src => renderDoc(readInput(src), mode))
      let result =
        if concat { outputs->Belt.Array.joinWith("\n", s => s) } else { outputs->Belt.Array.joinWith("\n\n", s => s) }
      switch outPath {
      | Some(p) => writeText(p, result)
      | None => Console.log(result)
      }
  | "validate" =>
      let allErrors = sources
        ->Belt.Array.map(src => validateDoc(readInput(src), mode))
        ->Belt.Array.reduce([], (acc, errs) => Belt.Array.concat(acc, errs))
      if Belt.Array.length(allErrors) == 0 {
        Console.log("ok")
      } else {
        allErrors->Belt.Array.forEach(e => Console.error(`${Int.toString(e.line)}: ${e.msg}`))
        exit(2)
      }
  | "ast" =>
      let outputs = sources->Belt.Array.map(src => astDoc(readInput(src), mode))
      let result = outputs->Belt.Array.joinWith("\n", s => s)
      switch outPath {
      | Some(p) => writeText(p, result)
      | None => Console.log(result)
      }
  | _ => usage()
  }
}
