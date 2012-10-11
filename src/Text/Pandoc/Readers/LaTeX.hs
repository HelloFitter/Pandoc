{-
Copyright (C) 2006-2012 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.LaTeX
   Copyright   : Copyright (C) 2006-2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.
-}
module Text.Pandoc.Readers.LaTeX ( readLaTeX,
                                   rawLaTeXInline,
                                   rawLaTeXBlock,
                                 ) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding ((<|>), many, optional, space)
import Data.Char ( chr, ord )
import Control.Monad
import Text.Pandoc.Builder
import Data.Char (isLetter, isPunctuation, isSpace)
import Control.Applicative
import Data.Monoid
import System.FilePath (replaceExtension)
import Data.List (intercalate)
import qualified Data.Map as M
import Control.Monad.Identity (Identity)
import Control.Monad.Trans ( lift )

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: PMonad m
          => ReaderOptions -- ^ Reader options
          -> String        -- ^ String to parse (assumes @'\n'@ line endings)
          -> m Pandoc
readLaTeX opts = readWith parseLaTeX def{ stateOptions = opts }

type LP m = Parser [Char] ParserState m

parseLaTeX :: PMonad m => LP m Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let title' = stateTitle st
  let authors' = stateAuthors st
  let date' = stateDate st
  return $ Pandoc (Meta title' authors' date') $ toList bs

anyControlSeq :: PMonad m => LP m String
anyControlSeq = do
  char '\\'
  next <- option '\n' anyChar
  name <- case next of
               '\n'           -> return ""
               c | isLetter c -> (c:) <$> (many letter <* optional sp)
                 | otherwise  -> return [c]
  return name

controlSeq :: PMonad m => String -> LP m String
controlSeq name = try $ do
  char '\\'
  case name of
        ""   -> mzero
        [c] | not (isLetter c) -> string [c]
        cs   -> string cs <* notFollowedBy letter <* optional sp
  return name

dimenarg :: PMonad m => LP m String
dimenarg = try $ do
  ch  <- option "" $ string "="
  num <- many1 digit
  dim <- oneOfStrings ["pt","pc","in","bp","cm","mm","dd","cc","sp"]
  return $ ch ++ num ++ dim

sp :: PMonad m => LP m ()
sp = skipMany1 $ satisfy (\c -> c == ' ' || c == '\t')
        <|> (try $ newline >>~ lookAhead anyChar >>~ notFollowedBy blankline)

isLowerHex :: Char -> Bool
isLowerHex x = x >= '0' && x <= '9' || x >= 'a' && x <= 'f'

tildeEscape :: PMonad m => LP m Char
tildeEscape = try $ do
  string "^^"
  c <- satisfy (\x -> x >= '\0' && x <= '\128')
  d <- if isLowerHex c
          then option "" $ count 1 (satisfy isLowerHex)
          else return ""
  if null d
     then case ord c of
           x | x >= 64 && x <= 127 -> return $ chr (x - 64)
             | otherwise           -> return $ chr (x + 64)
     else return $ chr $ read ('0':'x':c:d)

comment :: PMonad m => LP m ()
comment = do
  char '%'
  skipMany (satisfy (/='\n'))
  newline
  return ()

bgroup :: PMonad m => LP m ()
bgroup = () <$ char '{'
     <|> () <$ controlSeq "bgroup"
     <|> () <$ controlSeq "begingroup"

egroup :: PMonad m => LP m ()
egroup = () <$ char '}'
     <|> () <$ controlSeq "egroup"
     <|> () <$ controlSeq "endgroup"

grouped :: (PMonad m, Monoid a) => LP m a -> LP m a
grouped parser = try $ bgroup *> (mconcat <$> manyTill parser egroup)

braced :: PMonad m => LP m String
braced = bgroup *> (concat <$> manyTill
         (  many1 (satisfy (\c -> c /= '\\' && c /= '}' && c /= '{'))
        <|> try (string "\\}")
        <|> try (string "\\{")
        <|> try (string "\\\\")
        <|> ((\x -> "{" ++ x ++ "}") <$> braced)
        <|> count 1 anyChar
         ) egroup)

bracketed :: PMonad m => Monoid a => LP m a -> LP m a
bracketed parser = try $ char '[' *> (mconcat <$> manyTill parser (char ']'))

mathDisplay :: PMonad m => LP m String -> LP m Inlines
mathDisplay p = displayMath <$> (try p >>= applyMacros' . trim)

mathInline :: PMonad m => LP m String -> LP m Inlines
mathInline p = math <$> (try p >>= applyMacros')

mathChars :: PMonad m => LP m String
mathChars = concat <$>
  many (   many1 (satisfy (\c -> c /= '$' && c /='\\'))
      <|> (\c -> ['\\',c]) <$> (try $ char '\\' *> anyChar)
       )

double_quote :: PMonad m => LP m Inlines
double_quote = (doubleQuoted . mconcat) <$>
  (try $ string "``" *> manyTill inline (try $ string "''"))

single_quote :: PMonad m => LP m Inlines
single_quote = (singleQuoted . mconcat) <$>
  (try $ char '`' *> manyTill inline (try $ char '\'' >> notFollowedBy letter))

inline :: PMonad m => LP m Inlines
inline = (mempty <$ comment)
     <|> (space  <$ sp)
     <|> inlineText
     <|> inlineCommand
     <|> grouped inline
     <|> (char '-' *> option (str "-")
           ((char '-') *> option (str "–") (str "—" <$ char '-')))
     <|> double_quote
     <|> single_quote
     <|> (str "“" <$ try (string "``"))  -- nb. {``} won't be caught by double_quote
     <|> (str "”" <$ try (string "''"))
     <|> (str "‘" <$ char '`')           -- nb. {`} won't be caught by single_quote
     <|> (str "’" <$ char '\'')
     <|> (str "\160" <$ char '~')
     <|> (mathDisplay $ string "$$" *> mathChars <* string "$$")
     <|> (mathInline  $ char '$' *> mathChars <* char '$')
     <|> (superscript <$> (char '^' *> tok))
     <|> (subscript <$> (char '_' *> tok))
     <|> (guardEnabled Ext_literate_haskell *> char '|' *> doLHSverb)
     <|> (str . (:[]) <$> tildeEscape)
     <|> (str . (:[]) <$> oneOf "[]")
     <|> (str . (:[]) <$> oneOf "#&") -- TODO print warning?
     -- <|> (str <$> count 1 (satisfy (\c -> c /= '\\' && c /='\n' && c /='}' && c /='{'))) -- eat random leftover characters

inlines :: PMonad m => LP m Inlines
inlines = mconcat <$> many (notFollowedBy (char '}') *> inline)

block :: PMonad m => LP m Blocks
block = (mempty <$ comment)
    <|> (mempty <$ ((spaceChar <|> newline) *> spaces))
    <|> environment
    <|> mempty <$ macro -- TODO improve macros, make them work everywhere
    <|> blockCommand
    <|> paragraph
    <|> grouped block
    <|> (mempty <$ char '&')  -- loose & in table environment


blocks :: PMonad m => LP m Blocks
blocks = mconcat <$> many block

blockCommand :: PMonad m => LP m Blocks
blockCommand = try $ do
  name <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  star <- option "" (string "*" <* optional sp)
  let name' = name ++ star
  case M.lookup name' blockCommands of
       Just p      -> p
       Nothing     -> case M.lookup name blockCommands of
                           Just p    -> p
                           Nothing   -> mzero

inBrackets :: Inlines -> Inlines
inBrackets x = (str "[") <> x <> (str "]")

-- eat an optional argument and one or more arguments in braces
ignoreInlines :: PMonad m => String -> (String, LP m Inlines)
ignoreInlines name = (name, doraw <|> (mempty <$ optargs))
  where optargs = skipopts *> skipMany (try $ optional sp *> braced)
        contseq = '\\':name
        doraw = (rawInline "latex" . (contseq ++) . snd) <$>
                 (getOption readerParseRaw >>= guard >> (withRaw optargs))

ignoreBlocks :: PMonad m => String -> (String, LP m Blocks)
ignoreBlocks name = (name, doraw <|> (mempty <$ optargs))
  where optargs = skipopts *> skipMany (try $ optional sp *> braced)
        contseq = '\\':name
        doraw = (rawBlock "latex" . (contseq ++) . snd) <$>
                 (getOption readerParseRaw >>= guard >> (withRaw optargs))

blockCommands :: PMonad m => M.Map String (LP m Blocks)
blockCommands = M.fromList $
  [ ("par", mempty <$ skipopts)
  , ("title", mempty <$ (skipopts *> tok >>= addTitle))
  , ("subtitle", mempty <$ (skipopts *> tok >>= addSubtitle))
  , ("author", mempty <$ (skipopts *> authors))
  , ("usepackage", skipopts *> braced >>= include "usepackage")
  , ("include", skipopts *> braced >>= include "include")
  -- -- in letter class, temp. store address & sig as title, author
  , ("address", mempty <$ (skipopts *> tok >>= addTitle))
  , ("signature", mempty <$ (skipopts *> authors))
  , ("date", mempty <$ (skipopts *> tok >>= addDate))
  -- sectioning
  , ("chapter", updateState (\s -> s{ stateHasChapters = True }) *> section 0)
  , ("section", section 1)
  , ("subsection", section 2)
  , ("subsubsection", section 3)
  , ("paragraph", section 4)
  , ("subparagraph", section 5)
  -- beamer slides
  , ("frametitle", section 3)
  , ("framesubtitle", section 4)
  -- letters
  , ("opening", (para . trimInlines) <$> (skipopts *> tok))
  , ("closing", skipopts *> closing)
  --
  , ("rule", skipopts *> tok *> tok *> pure horizontalRule)
  , ("item", skipopts *> loose_item)
  , ("documentclass", skipopts *> braced *> preamble)
  , ("centerline", (para . trimInlines) <$> (skipopts *> tok))
  ] ++ map ignoreBlocks
  -- these commands will be ignored unless --parse-raw is specified,
  -- in which case they will appear as raw latex blocks
  [ "newcommand", "renewcommand", "newenvironment", "renewenvironment"
    -- newcommand, etc. should be parsed by macro, but we need this
    -- here so these aren't parsed as inline commands to ignore
  , "special", "pdfannot", "pdfstringdef"
  , "bibliography", "bibliographystyle"
  , "maketitle", "makeindex", "makeglossary"
  , "addcontentsline", "addtocontents", "addtocounter"
     -- \ignore{} is used conventionally in literate haskell for definitions
     -- that are to be processed by the compiler but not printed.
  , "ignore"
  , "hyperdef"
  , "noindent"
  , "markboth", "markright", "markleft"
  , "hspace", "vspace"
  ]

addTitle :: PMonad m => Inlines -> LP m ()
addTitle tit = updateState (\s -> s{ stateTitle = toList tit })

addSubtitle :: PMonad m => Inlines -> LP m ()
addSubtitle tit = updateState (\s -> s{ stateTitle = stateTitle s ++
                        toList (str ":" <> linebreak <> tit) })

authors :: PMonad m => LP m ()
authors = try $ do
  char '{'
  let oneAuthor = mconcat <$>
       many1 (notFollowedBy' (controlSeq "and") >>
               (inline <|> mempty <$ blockCommand))
               -- skip e.g. \vspace{10pt}
  auths <- sepBy oneAuthor (controlSeq "and")
  char '}'
  updateState (\s -> s { stateAuthors = map (normalizeSpaces . toList) auths })

addDate :: PMonad m => Inlines -> LP m ()
addDate dat = updateState (\s -> s{ stateDate = toList dat })

section :: PMonad m => Int -> LP m Blocks
section lvl = do
  hasChapters <- stateHasChapters `fmap` getState
  let lvl' = if hasChapters then lvl + 1 else lvl
  skipopts
  contents <- grouped inline
  return $ header lvl' contents

inlineCommand :: PMonad m => LP m Inlines
inlineCommand = try $ do
  name <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  guard $ not $ isBlockCommand name
  parseRaw <- getOption readerParseRaw
  star <- option "" (string "*")
  let name' = name ++ star
  let rawargs = withRaw (skipopts *> option "" dimenarg
                  *> many braced) >>= applyMacros' . snd
  let raw = if parseRaw
               then (rawInline "latex" . (('\\':name') ++)) <$> rawargs
               else mempty <$> rawargs
  case M.lookup name' inlineCommands of
       Just p      -> p <|> raw
       Nothing     -> case M.lookup name inlineCommands of
                           Just p    -> p <|> raw
                           Nothing   -> raw

unlessParseRaw :: PMonad m => LP m ()
unlessParseRaw = getOption readerParseRaw >>= guard . not

isBlockCommand :: String -> Bool
isBlockCommand s = maybe False (const True) $ M.lookup s (blockCommands :: M.Map String (LP Identity Blocks))

inlineCommands :: PMonad m => M.Map String (LP m Inlines)
inlineCommands = M.fromList $
  [ ("emph", emph <$> tok)
  , ("textit", emph <$> tok)
  , ("textsc", smallcaps <$> tok)
  , ("sout", strikeout <$> tok)
  , ("textsuperscript", superscript <$> tok)
  , ("textsubscript", subscript <$> tok)
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("slash", lit "/")
  , ("textbf", strong <$> tok)
  , ("ldots", lit "…")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("label", unlessParseRaw >> (inBrackets <$> tok))
  , ("ref", unlessParseRaw >> (inBrackets <$> tok))
  , ("(", mathInline $ manyTill anyChar (try $ string "\\)"))
  , ("[", mathDisplay $ manyTill anyChar (try $ string "\\]"))
  , ("ensuremath", mathInline $ braced)
  , ("P", lit "¶")
  , ("S", lit "§")
  , ("$", lit "$")
  , ("%", lit "%")
  , ("&", lit "&")
  , ("#", lit "#")
  , ("_", lit "_")
  , ("{", lit "{")
  , ("}", lit "}")
  -- old TeX commands
  , ("em", emph <$> inlines)
  , ("it", emph <$> inlines)
  , ("sl", emph <$> inlines)
  , ("bf", strong <$> inlines)
  , ("rm", inlines)
  , ("itshape", emph <$> inlines)
  , ("slshape", emph <$> inlines)
  , ("scshape", smallcaps <$> inlines)
  , ("bfseries", strong <$> inlines)
  , ("/", pure mempty) -- italic correction
  , ("aa", lit "å")
  , ("AA", lit "Å")
  , ("ss", lit "ß")
  , ("o", lit "ø")
  , ("O", lit "Ø")
  , ("L", lit "Ł")
  , ("l", lit "ł")
  , ("ae", lit "æ")
  , ("AE", lit "Æ")
  , ("pounds", lit "£")
  , ("euro", lit "€")
  , ("copyright", lit "©")
  , ("`", option (str "`") $ try $ tok >>= accent grave)
  , ("'", option (str "'") $ try $ tok >>= accent acute)
  , ("^", option (str "^") $ try $ tok >>= accent circ)
  , ("~", option (str "~") $ try $ tok >>= accent tilde)
  , ("\"", option (str "\"") $ try $ tok >>= accent umlaut)
  , (".", option (str ".") $ try $ tok >>= accent dot)
  , ("=", option (str "=") $ try $ tok >>= accent macron)
  , ("c", option (str "c") $ try $ tok >>= accent cedilla)
  , ("i", lit "i")
  , ("\\", linebreak <$ (optional (bracketed inline) *> optional sp))
  , (",", pure mempty)
  , ("@", pure mempty)
  , (" ", lit "\160")
  , ("ps", pure $ str "PS." <> space)
  , ("TeX", lit "TeX")
  , ("LaTeX", lit "LaTeX")
  , ("bar", lit "|")
  , ("textless", lit "<")
  , ("textgreater", lit ">")
  , ("thanks", (note . mconcat) <$> (char '{' *> manyTill block (char '}')))
  , ("footnote", (note . mconcat) <$> (char '{' *> manyTill block (char '}')))
  , ("verb", doverb)
  , ("lstinline", doverb)
  , ("texttt", (code . stringify . toList) <$> tok)
  , ("url", (unescapeURL <$> braced) >>= \url ->
       pure (link url "" (codeWith ("",["url"],[]) url)))
  , ("href", (unescapeURL <$> braced <* optional sp) >>= \url ->
       tok >>= \lab ->
         pure (link url "" lab))
  , ("includegraphics", skipopts *> (unescapeURL <$> braced) >>=
       (\src -> pure (image src "" (str "image"))))
  , ("cite", citation "cite" AuthorInText False)
  , ("citep", citation "citep" NormalCitation False)
  , ("citep*", citation "citep*" NormalCitation False)
  , ("citeal", citation "citeal" NormalCitation False)
  , ("citealp", citation "citealp" NormalCitation False)
  , ("citealp*", citation "citealp*" NormalCitation False)
  , ("autocite", citation "autocite" NormalCitation False)
  , ("footcite", inNote <$> citation "footcite" AuthorInText False)
  , ("parencite", citation "parencite" NormalCitation False)
  , ("supercite", citation "supercite" NormalCitation False)
  , ("footcitetext", inNote <$> citation "footcitetext" AuthorInText False)
  , ("citeyearpar", citation "citeyearpar" SuppressAuthor False)
  , ("citeyear", citation "citeyear" SuppressAuthor False)
  , ("autocite*", citation "autocite*" SuppressAuthor False)
  , ("cite*", citation "cite*" SuppressAuthor False)
  , ("parencite*", citation "parencite*" SuppressAuthor False)
  , ("textcite", citation "textcite" AuthorInText False)
  , ("citet", citation "citet" AuthorInText False)
  , ("citet*", citation "citet*" AuthorInText False)
  , ("citealt", citation "citealt" AuthorInText False)
  , ("citealt*", citation "citealt*" AuthorInText False)
  , ("textcites", citation "textcites" AuthorInText True)
  , ("cites", citation "cites" NormalCitation True)
  , ("autocites", citation "autocites" NormalCitation True)
  , ("footcites", inNote <$> citation "footcites" AuthorInText True)
  , ("parencites", citation "parencites" NormalCitation True)
  , ("supercites", citation "supercites" NormalCitation True)
  , ("footcitetexts", inNote <$> citation "footcitetexts" AuthorInText True)
  , ("Autocite", citation "Autocite" NormalCitation False)
  , ("Footcite", citation "Footcite" NormalCitation False)
  , ("Parencite", citation "Parencite" NormalCitation False)
  , ("Supercite", citation "Supercite" NormalCitation False)
  , ("Footcitetext", inNote <$> citation "Footcitetext" NormalCitation False)
  , ("Citeyearpar", citation "Citeyearpar" SuppressAuthor False)
  , ("Citeyear", citation "Citeyear" SuppressAuthor False)
  , ("Autocite*", citation "Autocite*" SuppressAuthor False)
  , ("Cite*", citation "Cite*" SuppressAuthor False)
  , ("Parencite*", citation "Parencite*" SuppressAuthor False)
  , ("Textcite", citation "Textcite" AuthorInText False)
  , ("Textcites", citation "Textcites" AuthorInText True)
  , ("Cites", citation "Cites" NormalCitation True)
  , ("Autocites", citation "Autocites" NormalCitation True)
  , ("Footcites", citation "Footcites" NormalCitation True)
  , ("Parencites", citation "Parencites" NormalCitation True)
  , ("Supercites", citation "Supercites" NormalCitation True)
  , ("Footcitetexts", inNote <$> citation "Footcitetexts" NormalCitation True)
  , ("citetext", complexNatbibCitation NormalCitation)
  , ("citeauthor", (try (tok *> optional sp *> controlSeq "citetext") *>
                        complexNatbibCitation AuthorInText)
                   <|> citation "citeauthor" AuthorInText False)
  ] ++ map ignoreInlines
  -- these commands will be ignored unless --parse-raw is specified,
  -- in which case they will appear as raw latex blocks:
  [ "index", "nocite" ]

inNote :: Inlines -> Inlines
inNote ils =
  note $ para $ ils <> str "."

unescapeURL :: String -> String
unescapeURL ('\\':x:xs) | isEscapable x = x:unescapeURL xs
  where isEscapable '%' = True
        isEscapable '#' = True
        isEscapable _   = False
unescapeURL (x:xs) = x:unescapeURL xs
unescapeURL [] = ""

doverb :: PMonad m => LP m Inlines
doverb = do
  marker <- anyChar
  code <$> manyTill (satisfy (/='\n')) (char marker)

doLHSverb :: PMonad m => LP m Inlines
doLHSverb = codeWith ("",["haskell"],[]) <$> manyTill (satisfy (/='\n')) (char '|')

lit :: PMonad m => String -> LP m Inlines
lit = pure . str

accent :: PMonad m => (Char -> Char) -> Inlines -> LP m Inlines
accent f ils =
  case toList ils of
       (Str (x:xs) : ys) -> return $ fromList $ (Str (f x : xs) : ys)
       []                -> mzero
       _                 -> return ils

grave :: Char -> Char
grave 'A' = 'À'
grave 'E' = 'È'
grave 'I' = 'Ì'
grave 'O' = 'Ò'
grave 'U' = 'Ù'
grave 'a' = 'à'
grave 'e' = 'è'
grave 'i' = 'ì'
grave 'o' = 'ò'
grave 'u' = 'ù'
grave c   = c

acute :: Char -> Char
acute 'A' = 'Á'
acute 'E' = 'É'
acute 'I' = 'Í'
acute 'O' = 'Ó'
acute 'U' = 'Ú'
acute 'Y' = 'Ý'
acute 'a' = 'á'
acute 'e' = 'é'
acute 'i' = 'í'
acute 'o' = 'ó'
acute 'u' = 'ú'
acute 'y' = 'ý'
acute 'C' = 'Ć'
acute 'c' = 'ć'
acute 'L' = 'Ĺ'
acute 'l' = 'ĺ'
acute 'N' = 'Ń'
acute 'n' = 'ń'
acute 'R' = 'Ŕ'
acute 'r' = 'ŕ'
acute 'S' = 'Ś'
acute 's' = 'ś'
acute 'Z' = 'Ź'
acute 'z' = 'ź'
acute c = c

circ :: Char -> Char
circ 'A' = 'Â'
circ 'E' = 'Ê'
circ 'I' = 'Î'
circ 'O' = 'Ô'
circ 'U' = 'Û'
circ 'a' = 'â'
circ 'e' = 'ê'
circ 'i' = 'î'
circ 'o' = 'ô'
circ 'u' = 'û'
circ 'C' = 'Ĉ'
circ 'c' = 'ĉ'
circ 'G' = 'Ĝ'
circ 'g' = 'ĝ'
circ 'H' = 'Ĥ'
circ 'h' = 'ĥ'
circ 'J' = 'Ĵ'
circ 'j' = 'ĵ'
circ 'S' = 'Ŝ'
circ 's' = 'ŝ'
circ 'W' = 'Ŵ'
circ 'w' = 'ŵ'
circ 'Y' = 'Ŷ'
circ 'y' = 'ŷ'
circ c = c

tilde :: Char -> Char
tilde 'A' = 'Ã'
tilde 'a' = 'ã'
tilde 'O' = 'Õ'
tilde 'o' = 'õ'
tilde 'I' = 'Ĩ'
tilde 'i' = 'ĩ'
tilde 'U' = 'Ũ'
tilde 'u' = 'ũ'
tilde 'N' = 'Ñ'
tilde 'n' = 'ñ'
tilde c   = c

umlaut :: Char -> Char
umlaut 'A' = 'Ä'
umlaut 'E' = 'Ë'
umlaut 'I' = 'Ï'
umlaut 'O' = 'Ö'
umlaut 'U' = 'Ü'
umlaut 'a' = 'ä'
umlaut 'e' = 'ë'
umlaut 'i' = 'ï'
umlaut 'o' = 'ö'
umlaut 'u' = 'ü'
umlaut c = c

dot :: Char -> Char
dot 'C' = 'Ċ'
dot 'c' = 'ċ'
dot 'E' = 'Ė'
dot 'e' = 'ė'
dot 'G' = 'Ġ'
dot 'g' = 'ġ'
dot 'I' = 'İ'
dot 'Z' = 'Ż'
dot 'z' = 'ż'
dot c = c

macron :: Char -> Char
macron 'A' = 'Ā'
macron 'E' = 'Ē'
macron 'I' = 'Ī'
macron 'O' = 'Ō'
macron 'U' = 'Ū'
macron 'a' = 'ā'
macron 'e' = 'ē'
macron 'i' = 'ī'
macron 'o' = 'ō'
macron 'u' = 'ū'
macron c = c

cedilla :: Char -> Char
cedilla 'c' = 'ç'
cedilla 'C' = 'Ç'
cedilla 's' = 'ş'
cedilla 'S' = 'Ş'
cedilla c = c

tok :: PMonad m => LP m Inlines
tok = try $ grouped inline <|> inlineCommand <|> str <$> (count 1 $ inlineChar)

opt :: PMonad m => LP m Inlines
opt = bracketed inline <* optional sp

skipopts :: PMonad m => LP m ()
skipopts = skipMany opt

inlineText :: PMonad m => LP m Inlines
inlineText = str <$> many1 inlineChar

inlineChar :: PMonad m => LP m Char
inlineChar = noneOf "\\$%^_&~#{}^'`-[] \t\n"

environment :: PMonad m => LP m Blocks
environment = do
  controlSeq "begin"
  name <- braced
  case M.lookup name environments of
       Just p      -> p <|> rawEnv name
       Nothing     -> rawEnv name

rawEnv :: PMonad m => String -> LP m Blocks
rawEnv name = do
  let addBegin x = "\\begin{" ++ name ++ "}" ++ x
  parseRaw <- getOption readerParseRaw
  if parseRaw
     then (rawBlock "latex" . addBegin) <$>
            (withRaw (env name blocks) >>= applyMacros' . snd)
     else env name blocks

include :: PMonad m => String -> String -> LP m Blocks
include name arg = do
  let fs = splitBy (==',') arg
  rest <- getInput
  let fs' = if name == "include"
               then map (flip replaceExtension ".tex") fs
               else map (flip replaceExtension ".sty") fs
  contents <- concat <$> mapM (lift . getFile) fs'
  setInput (contents ++ rest)
  return mempty

rawLaTeXBlock :: PMonad m => Parser [Char] ParserState m String
rawLaTeXBlock = snd <$> try (withRaw (environment <|> blockCommand))

rawLaTeXInline :: PMonad m => Parser [Char] ParserState m Inline
rawLaTeXInline = do
  (res, raw) <- withRaw inlineCommand
  if res == mempty
     then return (Str "")
     else RawInline "latex" <$> (applyMacros' raw)

environments :: PMonad m => M.Map String (LP m Blocks)
environments = M.fromList
  [ ("document", env "document" blocks <* skipMany anyChar)
  , ("letter", env "letter" letter_contents)
  , ("center", env "center" blocks)
  , ("tabular", env "tabular" simpTable)
  , ("quote", blockQuote <$> env "quote" blocks)
  , ("quotation", blockQuote <$> env "quotation" blocks)
  , ("verse", blockQuote <$> env "verse" blocks)
  , ("itemize", bulletList <$> listenv "itemize" (many item))
  , ("description", definitionList <$> listenv "description" (many descItem))
  , ("enumerate", ordered_list)
  , ("code", guardEnabled Ext_literate_haskell *>
      (codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
        verbEnv "code"))
  , ("verbatim", codeBlock <$> (verbEnv "verbatim"))
  , ("Verbatim", codeBlock <$> (verbEnv "Verbatim"))
  , ("lstlisting", codeBlock <$> (verbEnv "lstlisting"))
  , ("minted", liftA2 (\l c -> codeBlockWith ("",[l],[]) c)
            (grouped (many1 $ satisfy (/= '}'))) (verbEnv "minted"))
  , ("obeylines", parseFromString
                  (para . trimInlines . mconcat <$> many inline) =<<
                  intercalate "\\\\\n" . lines <$> verbEnv "obeylines")
  , ("displaymath", mathEnv Nothing "displaymath")
  , ("equation", mathEnv Nothing "equation")
  , ("equation*", mathEnv Nothing "equation*")
  , ("gather", mathEnv (Just "gathered") "gather")
  , ("gather*", mathEnv (Just "gathered") "gather*")
  , ("multline", mathEnv (Just "gathered") "multline")
  , ("multline*", mathEnv (Just "gathered") "multline*")
  , ("eqnarray", mathEnv (Just "aligned") "eqnarray")
  , ("eqnarray*", mathEnv (Just "aligned") "eqnarray*")
  , ("align", mathEnv (Just "aligned") "align")
  , ("align*", mathEnv (Just "aligned") "align*")
  , ("alignat", mathEnv (Just "aligned") "alignat")
  , ("alignat*", mathEnv (Just "aligned") "alignat*")
  ]

letter_contents :: PMonad m => LP m Blocks
letter_contents = do
  bs <- blocks
  st <- getState
  -- add signature (author) and address (title)
  let addr = case stateTitle st of
                  []   -> mempty
                  x    -> para $ trimInlines $ fromList x
  updateState $ \s -> s{ stateAuthors = [], stateTitle = [] }
  return $ addr <> bs -- sig added by \closing

closing :: PMonad m => LP m Blocks
closing = do
  contents <- tok
  st <- getState
  let sigs = case stateAuthors st of
                  []   -> mempty
                  xs   -> para $ trimInlines $ fromList
                               $ intercalate [LineBreak] xs
  return $ para (trimInlines contents) <> sigs

item :: PMonad m => LP m Blocks
item = blocks *> controlSeq "item" *> skipopts *> blocks

loose_item :: PMonad m => LP m Blocks
loose_item = do
  ctx <- stateParserContext `fmap` getState
  if ctx == ListItemState
     then mzero
     else return mempty

descItem :: PMonad m => LP m (Inlines, [Blocks])
descItem = do
  blocks -- skip blocks before item
  controlSeq "item"
  optional sp
  ils <- opt
  bs <- blocks
  return (ils, [bs])

env :: PMonad m => String -> LP m a -> LP m a
env name p = p <*
  (try (controlSeq "end" *> braced >>= guard . (== name))
    <?> ("\\end{" ++ name ++ "}"))

listenv :: PMonad m => String -> LP m a -> LP m a
listenv name p = try $ do
  oldCtx <- stateParserContext `fmap` getState
  updateState $ \st -> st{ stateParserContext = ListItemState }
  res <- env name p
  updateState $ \st -> st{ stateParserContext = oldCtx }
  return res

mathEnv :: PMonad m => Maybe String -> String -> LP m Blocks
mathEnv innerEnv name = para <$> mathDisplay (inner <$> verbEnv name)
   where inner x = case innerEnv of
                      Nothing -> x
                      Just y  -> "\\begin{" ++ y ++ "}\n" ++ x ++
                                    "\\end{" ++ y ++ "}"

verbEnv :: PMonad m => String -> LP m String
verbEnv name = do
  skipopts
  optional blankline
  let endEnv = try $ controlSeq "end" *> braced >>= guard . (== name)
  res <- manyTill anyChar endEnv
  return $ stripTrailingNewlines res

ordered_list :: PMonad m => LP m Blocks
ordered_list = do
  optional sp
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) $
                              try $ char '[' *> anyOrderedListMarker <* char ']'
  spaces
  optional $ try $ controlSeq "setlength" *> grouped (controlSeq "itemindent") *> braced
  spaces
  start <- option 1 $ try $ do controlSeq "setcounter"
                               grouped (string "enum" *> many1 (oneOf "iv"))
                               optional sp
                               num <- grouped (many1 digit)
                               spaces
                               return $ (read num + 1 :: Int)
  bs <- listenv "enumerate" (many item)
  return $ orderedListWith (start, style, delim) bs

paragraph :: PMonad m => LP m Blocks
paragraph = do
  x <- mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para $ trimInlines x

preamble :: PMonad m => LP m Blocks
preamble = mempty <$> manyTill preambleBlock beginDoc
  where beginDoc = lookAhead $ controlSeq "begin" *> string "{document}"
        preambleBlock =  (mempty <$ comment)
                     <|> (mempty <$ sp)
                     <|> (mempty <$ blanklines)
                     <|> (mempty <$ macro)
                     <|> blockCommand
                     <|> (mempty <$ anyControlSeq)
                     <|> (mempty <$ braced)
                     <|> (mempty <$ anyChar)

-------

-- citations

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks)   = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _ = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) =
  let k  = last ks
      s' = case s of
                (Str (c:_):_)
                  | not (isPunctuation c || isSpace c) -> Str "," : Space : s
                _                                      -> s
  in  init ks ++ [k {citationSuffix = citationSuffix k ++ s'}]
addSuffix _ _ = []

simpleCiteArgs :: PMonad m => LP m [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe $ toList <$> opt
  second <- optionMaybe $ toList <$> opt
  char '{'
  keys <- manyTill citationLabel (char '}')
  let (pre, suf) = case (first  , second ) of
        (Just s , Nothing) -> (mempty, s )
        (Just s , Just t ) -> (s , t )
        _                  -> (mempty, mempty)
      conv k = Citation { citationId      = k
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = NormalCitation
                        , citationHash    = 0
                        , citationNoteNum = 0
                        }
  return $ addPrefix pre $ addSuffix suf $ map conv keys

citationLabel :: PMonad m => LP m String
citationLabel  = trim <$>
  (many1 (satisfy $ \c -> c /=',' && c /='}') <* optional (char ',') <* optional sp)

cites :: PMonad m => CitationMode -> Bool -> LP m [Citation]
cites mode multi = try $ do
  cits <- if multi
             then many1 simpleCiteArgs
             else count 1 simpleCiteArgs
  let (c:cs) = concat cits
  return $ case mode of
        AuthorInText   -> c {citationMode = mode} : cs
        _              -> map (\a -> a {citationMode = mode}) (c:cs)

citation :: PMonad m => String -> CitationMode -> Bool -> LP m Inlines
citation name mode multi = do
  (c,raw) <- withRaw $ cites mode multi
  return $ cite c (rawInline "latex" $ "\\" ++ name ++ raw)

complexNatbibCitation :: PMonad m => CitationMode -> LP m Inlines
complexNatbibCitation mode = try $ do
  let ils = (toList . trimInlines . mconcat) <$>
              many (notFollowedBy (oneOf "\\};") >> inline)
  let parseOne = try $ do
                   skipSpaces
                   pref  <- ils
                   cit' <- inline -- expect a citation
                   let citlist = toList cit'
                   cits' <- case citlist of
                                 [Cite cs _] -> return cs
                                 _           -> mzero
                   suff  <- ils
                   skipSpaces
                   optional $ char ';'
                   return $ addPrefix pref $ addSuffix suff $ cits'
  (c:cits, raw) <- withRaw $ grouped parseOne
  return $ cite (c{ citationMode = mode }:cits)
           (rawInline "latex" $ "\\citetext" ++ raw)

-- tables

parseAligns :: PMonad m => LP m [Alignment]
parseAligns = try $ do
  char '{'
  optional $ char '|'
  let cAlign = AlignCenter <$ char 'c'
  let lAlign = AlignLeft <$ char 'l'
  let rAlign = AlignRight <$ char 'r'
  let alignChar = optional sp *> (cAlign <|> lAlign <|> rAlign)
  aligns' <- sepEndBy alignChar (optional $ char '|')
  spaces
  char '}'
  spaces
  return aligns'

hline :: PMonad m => LP m ()
hline = () <$ (try $ spaces >> controlSeq "hline")

lbreak :: PMonad m => LP m ()
lbreak = () <$ (try $ spaces *> controlSeq "\\")

amp :: PMonad m => LP m ()
amp = () <$ (try $ spaces *> char '&')

parseTableRow :: PMonad m => Int  -- ^ number of columns
              -> LP m [Blocks]
parseTableRow cols = try $ do
  let tableCellInline = notFollowedBy (amp <|> lbreak) >> inline
  let tableCell = (plain . trimInlines . mconcat) <$> many tableCellInline
  cells' <- sepBy tableCell amp
  guard $ length cells' == cols
  spaces
  return cells'

simpTable :: PMonad m => LP m Blocks
simpTable = try $ do
  spaces
  aligns <- parseAligns
  let cols = length aligns
  optional hline
  header' <- option [] $ try (parseTableRow cols <* lbreak <* hline)
  rows <- sepEndBy (parseTableRow cols) (lbreak <* optional hline)
  spaces
  let header'' = if null header'
                    then replicate cols mempty
                    else header'
  lookAhead $ controlSeq "end" -- make sure we're at end
  return $ table mempty (zip aligns (repeat 0)) header'' rows

