{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances,
             ScopedTypeVariables #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Parsing
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A utility library with parsers used in pandoc readers.
-}
module Text.Pandoc.Parsing ( (>>~),
                             anyLine,
                             many1Till,
                             notFollowedBy',
                             oneOfStrings,
                             spaceChar,
                             nonspaceChar,
                             skipSpaces,
                             blankline,
                             blanklines,
                             enclosed,
                             stringAnyCase,
                             parseFromString,
                             lineClump,
                             charsInBalanced,
                             romanNumeral,
                             emailAddress,
                             uri,
                             withHorizDisplacement,
                             withRaw,
                             escaped,
                             characterReference,
                             updateLastStrPos,
                             anyOrderedListMarker,
                             orderedListMarker,
                             charRef,
                             tableWith,
                             widthsFromIndices,
                             gridTableWith,
                             readWith,
                             testStringWith,
                             getOption,
                             guardEnabled,
                             guardDisabled,
                             ParserState (..),
                             defaultParserState,
                             HeaderType (..),
                             ParserContext (..),
                             QuoteContext (..),
                             NoteTable,
                             NoteTable',
                             KeyTable,
                             SubstTable,
                             Key (..),
                             toKey,
                             smartPunctuation,
                             withQuoteContext,
                             singleQuoteStart,
                             singleQuoteEnd,
                             doubleQuoteStart,
                             doubleQuoteEnd,
                             ellipses,
                             apostrophe,
                             dash,
                             nested,
                             macro,
                             applyMacros',
                             Parser,
                             PMonad (..),
                             F(..),
                             runF,
                             askF,
                             asksF,
                             -- * Re-exports from Text.Pandoc.Parsec
                             runParser,
                             parse,
                             anyToken,
                             getInput,
                             setInput,
                             unexpected,
                             char,
                             letter,
                             digit,
                             alphaNum,
                             skipMany,
                             skipMany1,
                             spaces,
                             space,
                             anyChar,
                             satisfy,
                             newline,
                             string,
                             count,
                             eof,
                             noneOf,
                             oneOf,
                             lookAhead,
                             notFollowedBy,
                             many,
                             many1,
                             manyTill,
                             (<|>),
                             (<?>),
                             choice,
                             try,
                             sepBy,
                             sepBy1,
                             sepEndBy,
                             sepEndBy1,
                             endBy,
                             endBy1,
                             option,
                             optional,
                             optionMaybe,
                             getState,
                             setState,
                             updateState,
                             getPosition,
                             setPosition,
                             sourceColumn,
                             sourceLine,
                             newPos,
                             token,
                             tokenPrim
                             )
where

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder (Blocks, Inlines)
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Data.Char ( toLower, toUpper, ord, isAscii, isAlphaNum, isDigit, isPunctuation )
import Data.List ( intercalate, transpose )
import Network.URI ( parseURI, URI (..), isAllowedInURI )
import Text.Pandoc.Shared
import qualified Data.Map as M
import Text.TeXMath.Macros (applyMacros, Macro, parseMacroDefinitions)
import Text.HTML.TagSoup.Entity ( lookupEntity )
import Data.Default
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import qualified Control.Exception as E
import qualified Text.Pandoc.UTF8 as UTF8

class Monad m => PMonad m where
  addMessage :: String -> m ()
  addMessage _ = return ()
  getFile    :: FilePath -> m String
  getFile    _ = return ""

instance PMonad IO where
  addMessage m = warn m
  getFile    f = E.catch (UTF8.readFile f) $ \(_ :: E.SomeException) ->
                   do addMessage $ "Could not read `" ++ f ++ "'; skipping."
                      return ""

instance PMonad (Writer String) where
  addMessage m = tell m
  getFile _    = return ""

instance PMonad Identity
instance PMonad Maybe

type Parser s u m = ParsecT s u m

newtype F a = F { unF :: Reader ParserState a } deriving (Monad, Functor)

runF :: F a -> ParserState -> a
runF = runReader . unF

askF :: F ParserState
askF = F ask

asksF :: (ParserState -> a) -> F a
asksF f = F $ asks f

instance Monoid a => Monoid (F a) where
  mempty = return mempty
  mappend = liftM2 mappend
  mconcat = liftM mconcat . sequence

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x

-- | Parse any line of text
anyLine :: Monad m => Parser [Char] st m [Char]
anyLine = manyTill anyChar newline

-- | Like @manyTill@, but reads at least one item.
many1Till :: Monad m => Parser [tok] st m a
          -> Parser [tok] st m end
          -> Parser [tok] st m [a]
many1Till p end = do
         first <- p
         rest <- manyTill p end
         return (first:rest)

-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: (Monad m, Show b) => Parser [a] st m b -> Parser [a] st m ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

-- | Parses one of a list of strings (tried in order).
oneOfStrings :: Monad m => [String] -> Parser [Char] st m String
oneOfStrings []   = fail "no strings"
oneOfStrings strs = do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x == c]
  case strs' of
       []  -> fail "not found"
       z | "" `elem` z -> return [c]
         | otherwise    -> (c:) `fmap` oneOfStrings strs'

-- | Parses a space or tab.
spaceChar :: Monad m => Parser [Char] st m Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Monad m => Parser [Char] st m Char
nonspaceChar = satisfy $ \x -> x /= '\t' && x /= '\n' && x /= ' ' && x /= '\r'

-- | Skips zero or more spaces or tabs.
skipSpaces :: Monad m => Parser [Char] st m ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Monad m => Parser [Char] st m Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: Monad m => Parser [Char] st m [Char]
blanklines = many1 blankline

-- | Parses material enclosed between start and end parsers.
enclosed :: Monad m => Parser [Char] st m t   -- ^ start parser
         -> Parser [Char] st m end  -- ^ end parser
         -> Parser [Char] st m a    -- ^ content parser (to be used repeatedly)
         -> Parser [Char] st m [a]
enclosed start end parser = try $
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: Monad m => [Char] -> Parser [Char] st m String
stringAnyCase [] = string ""
stringAnyCase (x:xs) = do
  firstChar <- char (toUpper x) <|> char (toLower x)
  rest <- stringAnyCase xs
  return (firstChar:rest)

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: Monad m => Parser [tok] st m a -> [tok] -> Parser [tok] st m a
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput str
  result <- parser
  setInput oldInput
  setPosition oldPos
  return result

-- | Parse raw line block up to and including blank lines.
lineClump :: Monad m => Parser [Char] st m String
lineClump = blanklines
          <|> (many1 (notFollowedBy blankline >> anyLine) >>= return . unlines)

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close, which must m be different. For example,
-- @charsInBalanced '(' ')' anyChar@ will parse "(hello (there))"
-- and return "hello (there)".
charsInBalanced :: Monad m => Char -> Char -> (Parser [Char] st m Char)
                -> Parser [Char] st m String
charsInBalanced open close parser = try $ do
  char open
  let isDelim c = c == open || c == close
  raw <- many $  many1 (notFollowedBy (satisfy isDelim) >> parser)
             <|> (do res <- charsInBalanced open close parser
                     return $ [open] ++ res ++ [close])
  char close
  return $ concat raw

-- old charsInBalanced would be:
-- charsInBalanced open close (noneOf "\n" <|> char '\n' >> notFollowedBy blankline)
-- old charsInBalanced' would be:
-- charsInBalanced open close anyChar

-- Auxiliary functions for romanNumeral:

lowercaseRomanDigits :: [Char]
lowercaseRomanDigits = ['i','v','x','l','c','d','m']

uppercaseRomanDigits :: [Char]
uppercaseRomanDigits = map toUpper lowercaseRomanDigits

-- | Parses a roman numeral (uppercase or lowercase), returns number.
romanNumeral :: Monad m => Bool                  -- ^ Uppercase if true
             -> Parser [Char] st m Int
romanNumeral upperCase = do
    let romanDigits = if upperCase
                         then uppercaseRomanDigits
                         else lowercaseRomanDigits
    lookAhead $ oneOf romanDigits
    let [one, five, ten, fifty, hundred, fivehundred, thousand] =
          map char romanDigits
    thousands <- many thousand >>= (return . (1000 *) . length)
    ninehundreds <- option 0 $ try $ hundred >> thousand >> return 900
    fivehundreds <- many fivehundred >>= (return . (500 *) . length)
    fourhundreds <- option 0 $ try $ hundred >> fivehundred >> return 400
    hundreds <- many hundred >>= (return . (100 *) . length)
    nineties <- option 0 $ try $ ten >> hundred >> return 90
    fifties <- many fifty >>= (return . (50 *) . length)
    forties <- option 0 $ try $ ten >> fifty >> return 40
    tens <- many ten >>= (return . (10 *) . length)
    nines <- option 0 $ try $ one >> ten >> return 9
    fives <- many five >>= (return . (5 *) . length)
    fours <- option 0 $ try $ one >> five >> return 4
    ones <- many one >>= (return . length)
    let total = thousands + ninehundreds + fivehundreds + fourhundreds +
                hundreds + nineties + fifties + forties + tens + nines +
                fives + fours + ones
    if total == 0
       then fail "not a roman numeral"
       else return total

-- Parsers for email addresses and URIs

emailChar :: Monad m => Parser [Char] st m Char
emailChar = alphaNum <|>
            satisfy (\c -> c == '-' || c == '+' || c == '_' || c == '.')

domainChar :: Monad m => Parser [Char] st m Char
domainChar = alphaNum <|> char '-'

domain :: Monad m => Parser [Char] st m [Char]
domain = do
  first <- many1 domainChar
  dom <- many1 $ try (char '.' >> many1 domainChar )
  return $ intercalate "." (first:dom)

-- | Parses an email address; returns original and corresponding
-- escaped mailto: URI.
emailAddress :: Monad m => Parser [Char] st m (String, String)
emailAddress = try $ do
  firstLetter <- alphaNum
  restAddr <- many emailChar
  let addr = firstLetter:restAddr
  char '@'
  dom <- domain
  let full = addr ++ '@':dom
  return (full, escapeURI $ "mailto:" ++ full)

-- | Parses a URI. Returns pair of original and URI-escaped version.
uri :: Monad m => Parser [Char] st m (String, String)
uri = try $ do
  let protocols = [ "http:", "https:", "ftp:", "file:", "mailto:",
                    "news:", "telnet:" ]
  lookAhead $ oneOfStrings protocols
  -- Scan non-ascii characters and ascii characters allowed in a URI.
  -- We allow punctuation except when followed by a space, since
  -- we don't want the trailing '.' in 'http://google.com.'
  let innerPunct = try $ satisfy isPunctuation >>~
                         notFollowedBy (newline <|> spaceChar)
  let uriChar = innerPunct <|>
                satisfy (\c -> not (isPunctuation c) &&
                            (not (isAscii c) || isAllowedInURI c))
  -- We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org)
  -- So we include balanced parens in the URL.
  let inParens = try $ do char '('
                          res <- many uriChar
                          char ')'
                          return $ '(' : res ++ ")"
  str <- liftM concat $ many1 $ inParens <|> count 1 (innerPunct <|> uriChar)
  str' <- option str $ char '/' >> return (str ++ "/")
  -- now see if they amount to an absolute URI
  case parseURI (escapeURI str') of
       Just uri' -> if uriScheme uri' `elem` protocols
                       then return (str', show uri')
                       else fail "not a URI"
       Nothing   -> fail "not a URI"

-- | Applies a parser, returns tuple of its results and its horizontal
-- displacement (the difference between the source column at the end
-- and the source column at the beginning). Vertical displacement
-- (source row) is ignored.
withHorizDisplacement :: Monad m => Parser [Char] st m a  -- ^ Parser to apply
                      -> Parser [Char] st m (a, Int) -- ^ (result, displacement)
withHorizDisplacement parser = do
  pos1 <- getPosition
  result <- parser
  pos2 <- getPosition
  return (result, sourceColumn pos2 - sourceColumn pos1)

-- | Applies a parser and returns the raw string that was parsed,
-- along with the value produced by the parser.
withRaw :: Monad m => Parser [Char] st m a -> Parser [Char] st m (a, [Char])
withRaw parser = do
  pos1 <- getPosition
  inp <- getInput
  result <- parser
  pos2 <- getPosition
  let (l1,c1) = (sourceLine pos1, sourceColumn pos1)
  let (l2,c2) = (sourceLine pos2, sourceColumn pos2)
  let inplines = take ((l2 - l1) + 1) $ lines inp
  let raw = case inplines of
                []   -> error "raw: inplines is null" -- shouldn't happen
                [l]  -> take (c2 - c1) l
                ls   -> unlines (init ls) ++ take (c2 - 1) (last ls)
  return (result, raw)

-- | Parses backslash, then applies character parser.
escaped :: Monad m => Parser [Char] st m Char  -- ^ Parser for character to escape
        -> Parser [Char] st m Char
escaped parser = try $ char '\\' >> parser

-- | Parse character entity.
characterReference :: Monad m => Parser [Char] st m Char
characterReference = try $ do
  char '&'
  ent <- many1Till nonspaceChar (char ';')
  case lookupEntity ent of
       Just c  -> return c
       Nothing -> fail "entity not found"

-- | Parses an uppercase roman numeral and returns (UpperRoman, number).
upperRoman :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
upperRoman = do
  num <- romanNumeral True
  return (UpperRoman, num)

-- | Parses a lowercase roman numeral and returns (LowerRoman, number).
lowerRoman :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
lowerRoman = do
  num <- romanNumeral False
  return (LowerRoman, num)

-- | Parses a decimal numeral and returns (Decimal, number).
decimal :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
decimal = do
  num <- many1 digit
  return (Decimal, read num)

-- | Parses a '@' and optional label and
-- returns (DefaultStyle, [next example number]).  The next
-- example number is incremented in parser state, and the label
-- (if present) is added to the label table.
exampleNum :: Monad m => Parser [Char] ParserState m (ListNumberStyle, Int)
exampleNum = do
  char '@'
  lab <- many (alphaNum <|> satisfy (\c -> c == '_' || c == '-'))
  st <- getState
  let num = stateNextExample st
  let newlabels = if null lab
                     then stateExamples st
                     else M.insert lab num $ stateExamples st
  updateState $ \s -> s{ stateNextExample = num + 1
                       , stateExamples    = newlabels }
  return (Example, num)

-- | Parses a '#' returns (DefaultStyle, 1).
defaultNum :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
defaultNum = do
  char '#'
  return (DefaultStyle, 1)

-- | Parses a lowercase letter and returns (LowerAlpha, number).
lowerAlpha :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
lowerAlpha = do
  ch <- oneOf ['a'..'z']
  return (LowerAlpha, ord ch - ord 'a' + 1)

-- | Parses an uppercase letter and returns (UpperAlpha, number).
upperAlpha :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
upperAlpha = do
  ch <- oneOf ['A'..'Z']
  return (UpperAlpha, ord ch - ord 'A' + 1)

-- | Parses a roman numeral i or I
romanOne :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
romanOne = (char 'i' >> return (LowerRoman, 1)) <|>
           (char 'I' >> return (UpperRoman, 1))

-- | Parses an ordered list m marker and returns list m attributes.
anyOrderedListMarker :: Monad m => Parser [Char] ParserState m ListAttributes
anyOrderedListMarker = choice
  [delimParser numParser | delimParser <- [inPeriod, inOneParen, inTwoParens],
                           numParser <- [decimal, exampleNum, defaultNum, romanOne,
                           lowerAlpha, lowerRoman, upperAlpha, upperRoman]]

-- | Parses a list m number (num) followed by a period, returns list m attributes.
inPeriod :: Monad m => Parser [Char] ParserState m (ListNumberStyle, Int)
         -> Parser [Char] ParserState m ListAttributes
inPeriod num = try $ do
  (style, start) <- num
  char '.'
  let delim = if style == DefaultStyle
                 then DefaultDelim
                 else Period
  return (start, style, delim)

-- | Parses a list m number (num) followed by a paren, returns list m attributes.
inOneParen :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
           -> Parser [Char] st m ListAttributes
inOneParen num = try $ do
  (style, start) <- num
  char ')'
  return (start, style, OneParen)

-- | Parses a list m number (num) enclosed in parens, returns list m attributes.
inTwoParens :: Monad m => Parser [Char] st m (ListNumberStyle, Int)
            -> Parser [Char] st m ListAttributes
inTwoParens num = try $ do
  char '('
  (style, start) <- num
  char ')'
  return (start, style, TwoParens)

-- | Parses an ordered list m marker with a given style and delimiter,
-- returns number.
orderedListMarker :: Monad m => ListNumberStyle
                  -> ListNumberDelim
                  -> Parser [Char] ParserState m Int
orderedListMarker style delim = do
  let num = defaultNum <|>  -- # can continue any kind of list
            case style of
               DefaultStyle -> decimal
               Example      -> exampleNum
               Decimal      -> decimal
               UpperRoman   -> upperRoman
               LowerRoman   -> lowerRoman
               UpperAlpha   -> upperAlpha
               LowerAlpha   -> lowerAlpha
  let context = case delim of
               DefaultDelim -> inPeriod
               Period       -> inPeriod
               OneParen     -> inOneParen
               TwoParens    -> inTwoParens
  (start, _, _) <- context num
  return start

-- | Parses a character reference and returns a Str element.
charRef :: Monad m => Parser [Char] st m Inline
charRef = do
  c <- characterReference
  return $ Str [c]

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.
tableWith :: Monad m => Parser [Char] ParserState m ([[Block]], [Alignment], [Int])
          -> ([Int] -> Parser [Char] ParserState m [[Block]])
          -> Parser [Char] ParserState m sep
          -> Parser [Char] ParserState m end
          -> Parser [Char] ParserState m Block
tableWith headerParser rowParser lineParser footerParser = try $ do
    (heads, aligns, indices) <- headerParser
    lines' <- rowParser indices `sepEndBy1` lineParser
    footerParser
    numColumns <- getOption readerColumns
    let widths = if (indices == [])
                    then replicate (length aligns) 0.0
                    else widthsFromIndices numColumns indices
    return $ Table [] aligns widths heads lines'

-- Calculate relative widths of table columns, based on indices
widthsFromIndices :: Int      -- Number of columns on terminal
                  -> [Int]    -- Indices
                  -> [Double] -- Fractional relative sizes of columns
widthsFromIndices _ [] = []
widthsFromIndices numColumns' indices =
  let numColumns = max numColumns' (if null indices then 0 else last indices)
      lengths' = zipWith (-) indices (0:indices)
      lengths  = reverse $
                 case reverse lengths' of
                      []       -> []
                      [x]      -> [x]
                      -- compensate for the fact that intercolumn
                      -- spaces are counted in widths of all columns
                      -- but the last...
                      (x:y:zs) -> if x < y && y - x <= 2
                                     then y:y:zs
                                     else x:y:zs
      totLength = sum lengths
      quotient = if totLength > numColumns
                   then fromIntegral totLength
                   else fromIntegral numColumns
      fracs = map (\l -> (fromIntegral l) / quotient) lengths in
  tail fracs

---

-- Parse a grid table:  starts with row of '-' on top, then header
-- (which may be grid), then the rows,
-- which may be grid, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
gridTableWith :: Monad m => Parser [Char] ParserState m [Block]   -- ^ Block list m parser
              -> Bool                                -- ^ Headerless table
              -> Parser [Char] ParserState m Block
gridTableWith blocks headless =
  tableWith (gridTableHeader headless blocks) (gridTableRow blocks)
            (gridTableSep '-') gridTableFooter

gridTableSplitLine :: [Int] -> String -> [String]
gridTableSplitLine indices line = map removeFinalBar $ tail $
  splitStringByIndices (init indices) $ trimr line

gridPart :: Monad m => Char -> Parser [Char] st m (Int, Int)
gridPart ch = do
  dashes <- many1 (char ch)
  char '+'
  return (length dashes, length dashes + 1)

gridDashedLines :: Monad m => Char -> Parser [Char] st m [(Int,Int)]
gridDashedLines ch = try $ char '+' >> many1 (gridPart ch) >>~ blankline

removeFinalBar :: String -> String
removeFinalBar =
  reverse . dropWhile (`elem` " \t") . dropWhile (=='|') . reverse

-- | Separator between rows of grid table.
gridTableSep :: Monad m => Char -> Parser [Char] ParserState m Char
gridTableSep ch = try $ gridDashedLines ch >> return '\n'

-- | Parse header for a grid table.
gridTableHeader :: Monad m => Bool -- ^ Headerless table
                -> Parser [Char] ParserState m [Block]
                -> Parser [Char] ParserState m ([[Block]], [Alignment], [Int])
gridTableHeader headless blocks = try $ do
  optional blanklines
  dashes <- gridDashedLines '-'
  rawContent  <- if headless
                    then return $ repeat ""
                    else many1
                         (notFollowedBy (gridTableSep '=') >> char '|' >>
                           many1Till anyChar newline)
  if headless
     then return ()
     else gridTableSep '=' >> return ()
  let lines'   = map snd dashes
  let indices  = scanl (+) 0 lines'
  let aligns   = replicate (length lines') AlignDefault
  -- Rst m does not have a notion of alignments
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else map (intercalate " ") $ transpose
                       $ map (gridTableSplitLine indices) rawContent
  heads <- mapM (parseFromString blocks) $ map trim rawHeads
  return (heads, aligns, indices)

gridTableRawLine :: Monad m => [Int] -> Parser [Char] ParserState m [String]
gridTableRawLine indices = do
  char '|'
  line <- many1Till anyChar newline
  return (gridTableSplitLine indices line)

-- | Parse row of grid table.
gridTableRow :: Monad m => Parser [Char] ParserState m [Block]
             -> [Int]
             -> Parser [Char] ParserState m [[Block]]
gridTableRow blocks indices = do
  colLines <- many1 (gridTableRawLine indices)
  let cols = map ((++ "\n") . unlines . removeOneLeadingSpace) $
               transpose colLines
  mapM (liftM compactifyCell . parseFromString blocks) cols

removeOneLeadingSpace :: [String] -> [String]
removeOneLeadingSpace xs =
  if all startsWithSpace xs
     then map (drop 1) xs
     else xs
   where startsWithSpace ""     = True
         startsWithSpace (y:_) = y == ' '

compactifyCell :: [Block] -> [Block]
compactifyCell bs = head $ compactify [bs]

-- | Parse footer for a grid table.
gridTableFooter :: Monad m => Parser [Char] ParserState m [Char]
gridTableFooter = blanklines

---

-- | Parse a string with a given parser and state.
readWith :: PMonad m
         => Parser [t] ParserState m a            -- ^ parser
         -> ParserState                           -- ^ initial state
         -> [t]                                   -- ^ input
         -> m a
readWith parser state input = do
    res <- runParserT parser state "source" input
    case res of
      Left err'    -> fail $ show err'
      Right result -> return result

-- | Parse a string with @parser@ (for testing).
testStringWith :: (Show a) => Parser [Char] ParserState IO a
               -> String
               -> IO ()
testStringWith parser str = UTF8.putStrLn . show =<< readWith parser def str

-- | Parsing options.
data ParserState = ParserState
    { stateOptions         :: ReaderOptions, -- ^ User options
      stateParserContext   :: ParserContext, -- ^ Inside list?
      stateQuoteContext    :: QuoteContext,  -- ^ Inside quoted environment?
      stateAllowLinks      :: Bool,          -- ^ Allow parsing of links
      stateMaxNestingLevel :: Int,           -- ^ Max # of nested Strong/Emph
      stateLastStrPos      :: Maybe SourcePos, -- ^ Position after last str parsed
      stateKeys            :: KeyTable,      -- ^ List m of reference keys (with fallbacks)
      stateSubstitutions   :: SubstTable,    -- ^ List m of substitution references
      stateNotes           :: NoteTable,     -- ^ List m of notes (raw bodies)
      stateNotes'          :: NoteTable',    -- ^ List m of notes (parsed bodies)
      stateTitle           :: [Inline],      -- ^ Title of document
      stateAuthors         :: [[Inline]],    -- ^ Authors of document
      stateDate            :: [Inline],      -- ^ Date of document
      stateHeaderTable     :: [HeaderType],  -- ^ Ordered list m of header types used
      stateNextExample     :: Int,           -- ^ Number of next example
      stateExamples        :: M.Map String Int, -- ^ Map from example labels to numbers
      stateHasChapters     :: Bool,          -- ^ True if \chapter encountered
      stateMacros          :: [Macro],       -- ^ List m of macros defined so far
      stateRstDefaultRole  :: String         -- ^ Current rst m default interpreted text role
    }

instance Default ParserState where
  def = defaultParserState

defaultParserState :: ParserState
defaultParserState =
    ParserState { stateOptions         = def,
                  stateParserContext   = NullState,
                  stateQuoteContext    = NoQuote,
                  stateAllowLinks      = True,
                  stateMaxNestingLevel = 6,
                  stateLastStrPos      = Nothing,
                  stateKeys            = M.empty,
                  stateSubstitutions   = M.empty,
                  stateNotes           = [],
                  stateNotes'          = [],
                  stateTitle           = [],
                  stateAuthors         = [],
                  stateDate            = [],
                  stateHeaderTable     = [],
                  stateNextExample     = 1,
                  stateExamples        = M.empty,
                  stateHasChapters     = False,
                  stateMacros          = [],
                  stateRstDefaultRole  = "title-reference"}

getOption :: Monad m => (ReaderOptions -> a) -> Parser s ParserState m a
getOption f = (f . stateOptions) `fmap` getState

-- | Succeed only if the extension is enabled.
guardEnabled :: Monad m => Extension -> Parser s ParserState m ()
guardEnabled ext = getOption readerExtensions >>= guard . Set.member ext

-- | Succeed only if the extension is disabled.
guardDisabled :: Monad m => Extension -> Parser s ParserState m ()
guardDisabled ext = getOption readerExtensions >>= guard . not . Set.member ext

data HeaderType
    = SingleHeader Char  -- ^ Single line of characters underneath
    | DoubleHeader Char  -- ^ Lines of characters above and below
    deriving (Eq, Show)

data ParserContext
    = ListItemState   -- ^ Used when running parser on list m item contents
    | NullState       -- ^ Default state
    deriving (Eq, Show)

data QuoteContext
    = InSingleQuote   -- ^ Used when parsing inside single quotes
    | InDoubleQuote   -- ^ Used when parsing inside double quotes
    | NoQuote         -- ^ Used when not parsing inside quotes
    deriving (Eq, Show)

type NoteTable = [(String, String)]

type NoteTable' = [(String, F Blocks)]  -- used in markdown reader

newtype Key = Key String deriving (Show, Read, Eq, Ord)

toKey :: String -> Key
toKey = Key . map toLower . unwords . words

type KeyTable = M.Map Key Target

type SubstTable = M.Map Key Inlines

-- | Fail unless we're in "smart typography" mode.
failUnlessSmart :: Monad m => Parser [tok] ParserState m ()
failUnlessSmart = getOption readerSmart >>= guard

smartPunctuation :: Monad m => Parser [Char] ParserState m Inline
                 -> Parser [Char] ParserState m Inline
smartPunctuation inlineParser = do
  failUnlessSmart
  choice [ quoted inlineParser, apostrophe, dash, ellipses ]

apostrophe :: Monad m => Parser [Char] ParserState m Inline
apostrophe = (char '\'' <|> char '\8217') >> return (Str "\x2019")

quoted :: Monad m => Parser [Char] ParserState m Inline
       -> Parser [Char] ParserState m Inline
quoted inlineParser = doubleQuoted inlineParser <|> singleQuoted inlineParser

withQuoteContext :: Monad m => QuoteContext
                 -> Parser [tok] ParserState m a
                 -> Parser [tok] ParserState m a
withQuoteContext context parser = do
  oldState <- getState
  let oldQuoteContext = stateQuoteContext oldState
  setState oldState { stateQuoteContext = context }
  result <- parser
  newState <- getState
  setState newState { stateQuoteContext = oldQuoteContext }
  return result

singleQuoted :: Monad m => Parser [Char] ParserState m Inline
             -> Parser [Char] ParserState m Inline
singleQuoted inlineParser = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $ many1Till inlineParser singleQuoteEnd >>=
    return . Quoted SingleQuote . normalizeSpaces

doubleQuoted :: Monad m => Parser [Char] ParserState m Inline
             -> Parser [Char] ParserState m Inline
doubleQuoted inlineParser = try $ do
  doubleQuoteStart
  withQuoteContext InDoubleQuote $ do
    contents <- manyTill inlineParser doubleQuoteEnd
    return . Quoted DoubleQuote . normalizeSpaces $ contents

failIfInQuoteContext :: Monad m => QuoteContext -> Parser [tok] ParserState m ()
failIfInQuoteContext context = do
  st <- getState
  if stateQuoteContext st == context
     then fail "already inside quotes"
     else return ()

charOrRef :: Monad m => [Char] -> Parser [Char] st m Char
charOrRef cs =
  oneOf cs <|> try (do c <- characterReference
                       guard (c `elem` cs)
                       return c)

updateLastStrPos :: Monad m => Parser [Char] ParserState m ()
updateLastStrPos = getPosition >>= \p ->
  updateState $ \s -> s{ stateLastStrPos = Just p }

singleQuoteStart :: Monad m => Parser [Char] ParserState m ()
singleQuoteStart = do
  failIfInQuoteContext InSingleQuote
  pos <- getPosition
  st <- getState
  -- single quote start can't be right after str
  guard $ stateLastStrPos st /= Just pos
  try $ do charOrRef "'\8216\145"
           notFollowedBy (oneOf ")!],;:-? \t\n")
           notFollowedBy (char '.') <|> lookAhead (string "..." >> return ())
           notFollowedBy (try (oneOfStrings ["s","t","m","ve","ll","re"] >>
                               satisfy (not . isAlphaNum)))
                               -- possess/contraction
           return ()

singleQuoteEnd :: Monad m => Parser [Char] st m ()
singleQuoteEnd = try $ do
  charOrRef "'\8217\146"
  notFollowedBy alphaNum

doubleQuoteStart :: Monad m => Parser [Char] ParserState m ()
doubleQuoteStart = do
  failIfInQuoteContext InDoubleQuote
  try $ do charOrRef "\"\8220\147"
           notFollowedBy (satisfy (\c -> c == ' ' || c == '\t' || c == '\n'))

doubleQuoteEnd :: Monad m => Parser [Char] st m ()
doubleQuoteEnd = do
  charOrRef "\"\8221\148"
  return ()

ellipses :: Monad m => Parser [Char] st m Inline
ellipses = do
  try (charOrRef "\8230\133") <|> try (string "..." >> return '…')
  return (Str "\8230")

dash :: Monad m => Parser [Char] ParserState m Inline
dash = do
  oldDashes <- getOption readerOldDashes
  if oldDashes
     then emDashOld <|> enDashOld
     else Str `fmap` (hyphenDash <|> emDash <|> enDash)

-- Two hyphens = en-dash, three = em-dash
hyphenDash :: Monad m => Parser [Char] st m String
hyphenDash = do
  try $ string "--"
  option "\8211" (char '-' >> return "\8212")

emDash :: Monad m => Parser [Char] st m String
emDash = do
  try (charOrRef "\8212\151")
  return "\8212"

enDash :: Monad m => Parser [Char] st m String
enDash = do
  try (charOrRef "\8212\151")
  return "\8211"

enDashOld :: Monad m => Parser [Char] st m Inline
enDashOld = do
  try (charOrRef "\8211\150") <|>
    try (char '-' >> lookAhead (satisfy isDigit) >> return '–')
  return (Str "\8211")

emDashOld :: Monad m => Parser [Char] st m Inline
emDashOld = do
  try (charOrRef "\8212\151") <|> (try $ string "--" >> optional (char '-') >> return '-')
  return (Str "\8212")

-- This is used to prevent exponential blowups for things like:
-- a**a*a**a*a**a*a**a*a**a*a**a*a**
nested :: Monad m => Parser s ParserState m a -> Parser s ParserState m a
nested p = do
  nestlevel <- stateMaxNestingLevel `fmap` getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

--
-- Macros
--

-- | Parse a \newcommand or \renewcommand macro definition.
macro :: Monad m => Parser [Char] ParserState m Block
macro = do
  apply <- getOption readerApplyMacros
  inp <- getInput
  case parseMacroDefinitions inp of
       ([], _)    -> mzero
       (ms, rest) -> do def' <- count (length inp - length rest) anyChar
                        if apply
                           then do
                             updateState $ \st ->
                               st { stateMacros = ms ++ stateMacros st }
                             return Null
                           else return $ RawBlock "latex" def'

-- | Apply current macros to string.
applyMacros' :: Monad m => String -> Parser [Char] ParserState m String
applyMacros' target = do
  apply <- getOption readerApplyMacros
  if apply
     then do macros <- liftM stateMacros getState
             return $ applyMacros macros target
     else return target

