module A2ML.Parser

import A2ML.Surface
import Data.String
import Data.List
import Data.List1

%default total

-- Helper types for parsing
public export
record ParseState where
  constructor MkParseState
  input : List String  -- Lines
  pos : Nat            -- Current line index

public export
data ParseError = MkParseError Nat String

-- Line classification
isBlank : String -> Bool
isBlank s = all isSpace (unpack s)

startsWithHash : String -> Bool
startsWithHash s =
  case unpack (ltrim s) of
    ('#' :: _) => True
    _ => False

startsWithDash : String -> Bool
startsWithDash s =
  case unpack (ltrim s) of
    ('-' :: ' ' :: _) => True
    ('*' :: ' ' :: _) => True
    _ => False

startsWithDirective : String -> Bool
startsWithDirective s =
  let trimmed = trim s
  in case unpack trimmed of
    ('@' :: _) => case reverse (unpack trimmed) of
      (':' :: _) => True
      _ => False
    _ => False

isEndDirective : String -> Bool
isEndDirective s = trim s == "@end"

-- Count leading hashes
countHashes : String -> Nat
countHashes s = go (unpack (ltrim s))
  where
    go : List Char -> Nat
    go [] = 0
    go ('#' :: xs) = S (go xs)
    go _ = 0

-- Extract heading text (after hashes)
headingText : String -> String
headingText s =
  let trimmed = ltrim s
      level = countHashes trimmed
      rest = drop level (unpack trimmed)
  in trim (pack rest)

-- Parse inline (simplified - no nested formatting yet)
parseInline : String -> List Inline
parseInline s = [SText s]  -- TODO: Handle emphasis, strong, links

-- Parse directive name and attrs
parseDirectiveName : String -> String
parseDirectiveName s =
  let trimmed = ltrim s
      withoutAt = case unpack trimmed of
        ('@' :: xs) => pack xs
        _ => trimmed
      withoutColon = case reverse (unpack withoutAt) of
        (':' :: xs) => pack (reverse xs)
        _ => withoutAt
  in case break (== '(') (unpack withoutColon) of
    (name, _) => trim (pack name)

-- Parse blocks from lines
mutual
  covering  -- Termination is complex due to mutual recursion
  parseBlocks : List String -> Nat -> (List SBlock, Nat)
  parseBlocks [] pos = ([], pos)
  parseBlocks lines pos =
    if pos >= length lines then ([], pos)
    else
      let line = case inBounds pos lines of
            Yes prf => index pos lines
            No _ => ""
      in if isBlank line then parseBlocks lines (S pos)
         else if startsWithHash line then
           let level = countHashes line
               text = headingText line
           in let (rest, newPos) = parseBlocks lines (S pos)
              in (SHeading (cast level) text :: rest, newPos)
         else if startsWithDirective line then
           let name = parseDirectiveName line
               (body, newPos) = parseDirectiveBody lines (S pos)
           in let (rest, finalPos) = parseBlocks lines newPos
              in (SDirective name [] body :: rest, finalPos)
         else if startsWithDash line then
           let (items, newPos) = parseListItems lines pos
           in let (rest, finalPos) = parseBlocks lines newPos
              in (SList items :: rest, finalPos)
         else
           let (para, newPos) = parseParagraph lines pos
           in let (rest, finalPos) = parseBlocks lines newPos
              in (SParagraph para :: rest, finalPos)

  covering
  parseDirectiveBody : List String -> Nat -> (List SBlock, Nat)
  parseDirectiveBody [] pos = ([], pos)
  parseDirectiveBody lines pos =
    if pos >= length lines then ([], pos)
    else
      let line = case inBounds pos lines of
            Yes prf => index pos lines
            No _ => ""
      in if isEndDirective line then ([], S pos)
         else
           let (blocks, newPos) = parseBlocks lines pos
           in if newPos > pos then
             if newPos >= length lines then (blocks, newPos)
             else
               let nextLine = case inBounds newPos lines of
                     Yes prf => index newPos lines
                     No _ => ""
               in if isEndDirective nextLine then (blocks, S newPos)
                  else parseDirectiveBody lines newPos
           else (blocks, S pos)  -- Advance even if no blocks parsed

  covering
  parseListItems : List String -> Nat -> (List (List Inline), Nat)
  parseListItems [] pos = ([], pos)
  parseListItems lines pos =
    if pos >= length lines then ([], pos)
    else
      let line = case inBounds pos lines of
            Yes prf => index pos lines
            No _ => ""
      in if startsWithDash line then
           let text = case unpack (ltrim line) of
                 (_ :: ' ' :: rest) => trim (pack rest)
                 _ => ""
               item = parseInline text
               (rest, newPos) = parseListItems lines (S pos)
           in (item :: rest, newPos)
         else ([], pos)

  covering
  parseParagraph : List String -> Nat -> (List Inline, Nat)
  parseParagraph [] pos = ([], pos)
  parseParagraph lines pos =
    if pos >= length lines then ([], pos)
    else
      let line = case inBounds pos lines of
            Yes prf => index pos lines
            No _ => ""
      in if isBlank line || startsWithHash line || startsWithDirective line || startsWithDash line
         then ([], pos)
         else
           let (rest, newPos) = parseParagraph lines (S pos)
               combined = if null rest then parseInline line
                          else parseInline (line ++ " ") ++ rest
           in (combined, newPos)

-- Main parse function
export covering
parse : String -> Either ParseError SDoc
parse input =
  let inputLines = lines input
      (blocks, _) = parseBlocks inputLines 0
  in Right (MkSDoc blocks)

-- Render back to string (for testing)
export
renderInline : Inline -> String
renderInline (SText t) = t
renderInline (SEmph t) = "*" ++ t ++ "*"
renderInline (SStrong t) = "**" ++ t ++ "**"
renderInline (SLink label url) = "[" ++ label ++ "](" ++ url ++ ")"

export covering  -- Mutual recursion with render
renderBlock : SBlock -> String
renderBlock (SHeading level text) =
  pack (replicate (cast level) '#') ++ " " ++ text
renderBlock (SParagraph inlines) =
  concat (map renderInline inlines)
renderBlock (SList items) =
  unlines (map (\xs => "- " ++ concat (map renderInline xs)) items)
renderBlock (SDirective name attrs body) =
  "@" ++ name ++ ":\n" ++ unlines (map renderBlock body) ++ "@end"

export covering
render : SDoc -> String
render (MkSDoc blocks) = unlines (map renderBlock blocks)
