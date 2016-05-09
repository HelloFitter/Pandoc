{-
Copyright (C) 2007-2015 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Ms
   Copyright   : Copyright (C) 2007-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to groff man page format.

-}
module Text.Pandoc.Writers.Ms ( writeMs ) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Readers.TeXMath
import Text.Printf ( printf )
import Data.List ( stripPrefix, intersperse, intercalate )
import Data.Maybe (fromMaybe)
import Text.Pandoc.Pretty
import Text.Pandoc.Builder (deleteMeta)
import Control.Monad.State
import Data.Char ( isDigit )

type Notes = [[Block]]
data WriterState = WriterState { stNotes  :: Notes
                               , stHasTables :: Bool }

-- | Convert Pandoc to Ms.
writeMs :: WriterOptions -> Pandoc -> String
writeMs opts document = evalState (pandocToMs opts document) (WriterState [] False)

-- | Return groff man representation of document.
pandocToMs :: WriterOptions -> Pandoc -> State WriterState String
pandocToMs opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render' = render colwidth
  titleText <- inlineListToMs opts $ docTitle meta
  let title' = render' titleText
  let setFieldsFromTitle =
       case break (== ' ') title' of
           (cmdName, rest) -> case reverse cmdName of
                                   (')':d:'(':xs) | isDigit d ->
                                     defField "title" (reverse xs) .
                                     defField "section" [d] .
                                     case splitBy (=='|') rest of
                                          (ft:hds) ->
                                            defField "footer" (trim ft) .
                                            defField "header"
                                               (trim $ concat hds)
                                          [] -> id
                                   _  -> defField "title" title'
  metadata <- metaToJSON opts
              (fmap (render colwidth) . blockListToMs opts)
              (fmap (render colwidth) . inlineListToMs opts)
              $ deleteMeta "title" meta
  body <- blockListToMs opts blocks
  notes <- liftM stNotes get
  notes' <- notesToMs opts (reverse notes)
  let main = render' $ body $$ notes' $$ text ""
  hasTables <- liftM stHasTables get
  let context = defField "body" main
              $ setFieldsFromTitle
              $ defField "has-tables" hasTables
              $ defField "hyphenate" True
              $ defField "pandoc-version" pandocVersion
              $ metadata
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main

-- | Return man representation of notes.
notesToMs :: WriterOptions -> [[Block]] -> State WriterState Doc
notesToMs opts notes =
  if null notes
     then return empty
     else mapM (\(num, note) -> noteToMs opts num note) (zip [1..] notes) >>=
          return . (text ".SH NOTES" $$) . vcat

-- | Return man representation of a note.
noteToMs :: WriterOptions -> Int -> [Block] -> State WriterState Doc
noteToMs opts num note = do
  contents <- blockListToMs opts note
  let marker = cr <> text ".SS " <> brackets (text (show num))
  return $ marker $$ contents

-- | Association list of characters to escape.
manEscapes :: [(Char, String)]
manEscapes = [ ('\160', "\\ ")
             , ('\'', "\\[aq]")
             , ('’', "'")
             , ('\x2014', "\\[em]")
             , ('\x2013', "\\[en]")
             , ('\x2026', "\\&...")
             ] ++ backslashEscapes "-@\\"

-- | Escape special characters for Ms.
escapeString :: String -> String
escapeString = escapeStringUsing manEscapes

-- | Escape a literal (code) section for Ms.
escapeCode :: String -> String
escapeCode = concat . intersperse "\n" . map escapeLine . lines  where
  escapeLine codeline =
    case escapeStringUsing (manEscapes ++ backslashEscapes "\t ") codeline of
      a@('.':_) -> "\\&" ++ a
      b       -> b

-- We split inline lists into sentences, and print one sentence per
-- line.  groff/troff treats the line-ending period differently.
-- See http://code.google.com/p/pandoc/issues/detail?id=148.

-- | Returns the first sentence in a list of inlines, and the rest.
breakSentence :: [Inline] -> ([Inline], [Inline])
breakSentence [] = ([],[])
breakSentence xs =
  let isSentenceEndInline (Str ys@(_:_)) | last ys == '.' = True
      isSentenceEndInline (Str ys@(_:_)) | last ys == '?' = True
      isSentenceEndInline (LineBreak) = True
      isSentenceEndInline _         = False
      (as, bs) = break isSentenceEndInline xs
  in  case bs of
           []             -> (as, [])
           [c]            -> (as ++ [c], [])
           (c:Space:cs)   -> (as ++ [c], cs)
           (c:SoftBreak:cs) -> (as ++ [c], cs)
           (Str ".":Str (')':ys):cs) -> (as ++ [Str ".", Str (')':ys)], cs)
           (x@(Str ('.':')':_)):cs) -> (as ++ [x], cs)
           (LineBreak:x@(Str ('.':_)):cs) -> (as ++[LineBreak], x:cs)
           (c:cs)         -> (as ++ [c] ++ ds, es)
              where (ds, es) = breakSentence cs

-- | Split a list of inlines into sentences.
splitSentences :: [Inline] -> [[Inline]]
splitSentences xs =
  let (sent, rest) = breakSentence xs
  in  if null rest then [sent] else sent : splitSentences rest

-- | Convert Pandoc block element to man.
blockToMs :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc
blockToMs _ Null = return empty
blockToMs opts (Div _ bs) = blockListToMs opts bs
blockToMs opts (Plain inlines) =
  liftM vcat $ mapM (inlineListToMs opts) $ splitSentences inlines
blockToMs opts (Para inlines) = do
  contents <- liftM vcat $ mapM (inlineListToMs opts) $
    splitSentences inlines
  return $ text ".LP" $$ contents
blockToMs _ (RawBlock f str)
  | f == Format "man" = return $ text str
  | otherwise         = return empty
blockToMs _ HorizontalRule = return $ text ".PP" $$ text "   *   *   *   *   *"
blockToMs opts (Header level _ inlines) = do
  contents <- inlineListToMs opts inlines
  let heading = if writerNumberSections opts
                   then ".NH"
                   else ".SH"
  return $ text heading <> space <> text (show level) $$ contents
blockToMs _ (CodeBlock _ str) = return $
  text ".IP" $$
  text ".nf" $$
  text "\\f[C]" $$
  text (escapeCode str) $$
  text "\\f[]" $$
  text ".fi"
blockToMs opts (BlockQuote blocks) = do
  contents <- blockListToMs opts blocks
  return $ text ".RS" $$ contents $$ text ".RE"
blockToMs opts (Table caption alignments widths headers rows) =
  let aligncode AlignLeft    = "l"
      aligncode AlignRight   = "r"
      aligncode AlignCenter  = "c"
      aligncode AlignDefault = "l"
  in do
  caption' <- inlineListToMs opts caption
  modify $ \st -> st{ stHasTables = True }
  let iwidths = if all (== 0) widths
                   then repeat ""
                   else map (printf "w(%0.1fn)" . (70 *)) widths
  -- 78n default width - 8n indent = 70n
  let coldescriptions = text $ intercalate " "
                        (zipWith (\align width -> aligncode align ++ width)
                        alignments iwidths) ++ "."
  colheadings <- mapM (blockListToMs opts) headers
  let makeRow cols = text "T{" $$
                     (vcat $ intersperse (text "T}@T{") cols) $$
                     text "T}"
  let colheadings' = if all null headers
                        then empty
                        else makeRow colheadings $$ char '_'
  body <- mapM (\row -> do
                         cols <- mapM (blockListToMs opts) row
                         return $ makeRow cols) rows
  return $ text ".PP" $$ caption' $$
           text ".TS" $$ text "tab(@);" $$ coldescriptions $$
           colheadings' $$ vcat body $$ text ".TE"

blockToMs opts (BulletList items) = do
  contents <- mapM (bulletListItemToMs opts) items
  return (vcat contents)
blockToMs opts (OrderedList attribs items) = do
  let markers = take (length items) $ orderedListMarkers attribs
  let indent = 1 + (maximum $ map length markers)
  contents <- mapM (\(num, item) -> orderedListItemToMs opts num indent item) $
              zip markers items
  return (vcat contents)
blockToMs opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMs opts) items
  return (vcat contents)

-- | Convert bullet list item (list of blocks) to man.
bulletListItemToMs :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToMs _ [] = return empty
bulletListItemToMs opts ((Para first):rest) =
  bulletListItemToMs opts ((Plain first):rest)
bulletListItemToMs opts ((Plain first):rest) = do
  first' <- blockToMs opts (Plain first)
  rest' <- blockListToMs opts rest
  let first'' = text ".IP \\[bu] 2" $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 2" $$ rest' $$ text ".RE"
  return (first'' $$ rest'')
bulletListItemToMs opts (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  return $ text "\\[bu] .RS 2" $$ first' $$ rest' $$ text ".RE"

-- | Convert ordered list item (a list of blocks) to man.
orderedListItemToMs :: WriterOptions -- ^ options
                          -> String   -- ^ order marker for list item
                          -> Int      -- ^ number of spaces to indent
                          -> [Block]  -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToMs _ _ _ [] = return empty
orderedListItemToMs opts num indent ((Para first):rest) =
  orderedListItemToMs opts num indent ((Plain first):rest)
orderedListItemToMs opts num indent (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  let num' = printf ("%" ++ show (indent - 1) ++ "s") num
  let first'' = text (".IP \"" ++ num' ++ "\" " ++ show indent) $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 4" $$ rest' $$ text ".RE"
  return $ first'' $$ rest''

-- | Convert definition list item (label, list of blocks) to man.
definitionListItemToMs :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState Doc
definitionListItemToMs opts (label, defs) = do
  labelText <- inlineListToMs opts label
  contents <- if null defs
                 then return empty
                 else liftM vcat $ forM defs $ \blocks -> do
                        let (first, rest) = case blocks of
                              ((Para x):y) -> (Plain x,y)
                              (x:y)        -> (x,y)
                              []           -> error "blocks is null"
                        rest' <- liftM vcat $
                                  mapM (\item -> blockToMs opts item) rest
                        first' <- blockToMs opts first
                        return $ first' $$ text ".RS" $$ rest' $$ text ".RE"
  return $ text ".TP" $$ nowrap (text ".B " <> labelText) $$ contents

-- | Convert list of Pandoc block elements to man.
blockListToMs :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc
blockListToMs opts blocks =
  mapM (blockToMs opts) blocks >>= (return . vcat)

-- | Convert list of Pandoc inline elements to man.
inlineListToMs :: WriterOptions -> [Inline] -> State WriterState Doc
-- if list starts with ., insert a zero-width character \& so it
-- won't be interpreted as markup if it falls at the beginning of a line.
inlineListToMs opts lst@(Str ('.':_) : _) = mapM (inlineToMs opts) lst >>=
  (return . (text "\\&" <>)  . hcat)
inlineListToMs opts lst = mapM (inlineToMs opts) lst >>= (return . hcat)

-- | Convert Pandoc inline element to man.
inlineToMs :: WriterOptions -> Inline -> State WriterState Doc
inlineToMs opts (Span _ ils) = inlineListToMs opts ils
inlineToMs opts (Emph lst) = do
  contents <- inlineListToMs opts lst
  return $ text "\\f[I]" <> contents <> text "\\f[]"
inlineToMs opts (Strong lst) = do
  contents <- inlineListToMs opts lst
  return $ text "\\f[B]" <> contents <> text "\\f[]"
inlineToMs opts (Strikeout lst) = do
  contents <- inlineListToMs opts lst
  return $ text "[STRIKEOUT:" <> contents <> char ']'
inlineToMs opts (Superscript lst) = do
  contents <- inlineListToMs opts lst
  return $ char '^' <> contents <> char '^'
inlineToMs opts (Subscript lst) = do
  contents <- inlineListToMs opts lst
  return $ char '~' <> contents <> char '~'
inlineToMs opts (SmallCaps lst) = inlineListToMs opts lst -- not supported
inlineToMs opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMs opts lst
  return $ char '`' <> contents <> char '\''
inlineToMs opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMs opts lst
  return $ text "\\[lq]" <> contents <> text "\\[rq]"
inlineToMs opts (Cite _ lst) =
  inlineListToMs opts lst
inlineToMs _ (Code _ str) =
  return $ text $ "\\f[C]" ++ escapeCode str ++ "\\f[]"
inlineToMs _ (Str str) = return $ text $ escapeString str
inlineToMs opts (Math InlineMath str) =
  inlineListToMs opts $ texMathToInlines InlineMath str
inlineToMs opts (Math DisplayMath str) = do
  contents <- inlineListToMs opts $ texMathToInlines DisplayMath str
  return $ cr <> text ".RS" $$ contents $$ text ".RE"
inlineToMs _ (RawInline f str)
  | f == Format "man" = return $ text str
  | otherwise         = return empty
inlineToMs _ (LineBreak) = return $
  cr <> text ".PD 0" $$ text ".P" $$ text ".PD" <> cr
inlineToMs _ SoftBreak = return space
inlineToMs _ Space = return space
inlineToMs opts (Link _ txt (src, _)) = do
  linktext <- inlineListToMs opts txt
  let srcSuffix = fromMaybe src (stripPrefix "mailto:" src)
  return $ case txt of
           [Str s]
             | escapeURI s == srcSuffix ->
                                 char '<' <> text srcSuffix <> char '>'
           _                  -> linktext <> text " (" <> text src <> char ')'
inlineToMs opts (Image attr alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) ||
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMs opts (Link attr txt (source, tit))
  return $ char '[' <> text "IMAGE: " <> linkPart <> char ']'
inlineToMs _ (Note contents) = do
  -- add to notes in state
  modify $ \st -> st{ stNotes = contents : stNotes st }
  notes <- liftM stNotes get
  let ref = show $ (length notes)
  return $ char '[' <> text ref <> char ']'

