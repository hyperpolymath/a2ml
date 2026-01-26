module DemoBuild

import System
import System.File
import Ddraig

main : IO ()
main = do
  let inputPath = "prototype/ddraig/content/demo.md"
  let outputPath = "docs/demo.html"
  content <- readFile inputPath
  let (fm, body) = parseFrontmatter content
  let htmlBody = parseMarkdown body
  let htmlOut = applyTemplate fm htmlBody
  writeFile outputPath htmlOut
  putStrLn ("Wrote: " ++ outputPath)
