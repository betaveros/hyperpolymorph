import qualified Data.Map as M
import Control.Arrow
import Control.Applicative ((<$>))
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- utility HTML-making functions

makeSpanTag cls cont =
	"<span class='" ++ cls ++ "'>" ++ cont ++ "</span>"
makeStrongTag cont =
	"<strong>" ++ cont ++ "</strong>"

-- parsing

isId c = isAlphaNum c || c == '_'
idChar :: CharParser Char
idChar = satisfy isId

isKeyChar c = isAlphaNum c || c == '-'
keyChar :: CharParser Char
keyChar = satisfy isKeyChar

toKey = intercalate "-" . filter (isAlphaNum . head) . groupBy ((==) `on` isAlphaNum)

type CharParser = Parsec [Char] ()

bqAtom :: CharParser String
bqAtom = bqSpecial <|> plainAtom

plainAtom :: CharParser String
plainAtom = htmlEscaped <|> htmlNewline <|> many1 idChar <|> fmap return (noneOf "`")

htmlEscaped :: CharParser String
htmlEscaped = esc '&' "&amp;" <|> esc '<' "&lt;" <|> esc '>' "&gt;"
	where esc c s = char c >> return s

htmlNewline :: CharParser String
htmlNewline = do
	char '\n'
	indent <- many (char ' ')
	return $ "<br />" ++ concat (replicate (length indent) "&nbsp;")

makeNote cont = makeSpanTag "note" ("(" ++ cont ++ ")")

bqSpecial :: CharParser String
bqSpecial = try $ char '`' >>
	((char '`' >> return "`")
	<|> (char '[' >> return "[")
	<|> (char ']' >> return "]")
	<|> (char '*' >> fmap (makeSpanTag "key") bqGroup)
	<|> (char '+' >> fmap (makeSpanTag "op") bqGroup)
	<|> (char '$' >> fmap (makeSpanTag "cli") bqGroup)
	<|> (char '.' >> fmap (makeSpanTag "builtin") bqGroup)
	<|> (char '=' >> fmap (makeSpanTag "val") bqGroup)
	<|> (char '%' >> fmap (makeSpanTag "sval") bqGroup)
	<|> (char '!' >> fmap (makeSpanTag "error") bqGroup)
	<|> (char '#' >> fmap (makeSpanTag "preproc") bqGroup)
	<|> (fmap makeNote $ plainMatching '(' ')')
	)

bqMatching :: Char -> Char -> CharParser String
bqMatching lc rc = do
	char lc
	ss <- manyTill (((\x -> [lc] ++ x ++ [rc]) <$> bqMatching lc rc) <|> bqAtom) (char rc)
	return $ concat ss

plainMatching :: Char -> Char -> CharParser String
plainMatching lc rc = do
	char lc
	ss <- manyTill (((\x -> [lc] ++ x ++ [rc]) <$> plainMatching lc rc) <|> plainAtom) (char rc)
	return $ concat ss

bqGroup :: CharParser String
bqGroup = (bqMatching '[' ']') <|> bqAtom

bqEntry :: CharParser (String, String)
bqEntry = do
	string "`e"
	many1 (oneOf " \t")
	name <- many1 keyChar
	many1 space
	contAtoms <- manyTill bqAtom (eof <|> void (try $ lookAhead (spaces >> string "`e")))
	spaces
	return (name, concat contAtoms)

type EntryMap = M.Map String String

bqEntryMap :: CharParser EntryMap
bqEntryMap = do
	entries <- many bqEntry
	return . M.fromList $ filter (not . (all isSpace) . snd) entries

-- parse the entry associations used to create final output

bqEntryAssoc :: CharParser (String, String)
bqEntryAssoc = do
	key <- many1 keyChar
	char ':'
	many1 (oneOf " \t")
	desc <- many1 (noneOf "\n")
	newline
	return (key, desc)

bqEntryHeading :: CharParser String
bqEntryHeading = do
	char '#'
	spaces
	heading <- many1 (noneOf "\n")
	newline
	return heading

type Section = (String, [(String, String)])

bqEntrySection :: CharParser Section
bqEntrySection = do
	heading <- bqEntryHeading
	assocs <- many bqEntryAssoc
	return (heading, assocs)

bqSectionList :: CharParser [Section]
bqSectionList = many bqEntrySection

type NamedEntryMap = (String, EntryMap)

-- make everything together!

buildTd :: String -> NamedEntryMap -> String
buildTd key (name,emap) = case M.lookup key emap of
	Nothing -> "<td class='empty " ++ name ++ "'></td>"
	Just s -> "<td class='cont " ++ name ++ "'>" ++ s ++ "</td>"

buildRow :: (String, String) -> [NamedEntryMap] -> String
buildRow (key, desc) emaps =
	"<tr id='" ++ key ++ "'><td class='desc'><div class='pd'><a href='#" ++ key ++ "' class='perm'>#</a></div> " ++ desc ++ "</td>" ++ concatMap (buildTd key) emaps ++ "</tr>"

makeThRow colspan key cont =
	"<tr><td class='invis' rowspan='2'></td><th colspan='" ++ show colspan ++ "' class='section-header' id='" ++ key ++ "'>" ++ cont ++ "</th></tr>"

buildColumnHeads :: [NamedEntryMap] -> String
buildColumnHeads emaps =
	"<tr>" ++ concat ["<th class='" ++ name ++ "'>" ++ name ++ "</th>" | (name,_) <- emaps] ++ "</tr>"

buildSection :: Section -> [NamedEntryMap] -> String
buildSection (heading, assocs) emaps = unlines (
	makeThRow (length emaps) (toKey heading) heading :
	buildColumnHeads emaps : [
		buildRow assoc emaps | assoc <- assocs
	])

buildTable :: [Section] -> [NamedEntryMap] -> String
buildTable secs emaps = unlines (["<table>"]
	++ [buildSection sec emaps | sec <- secs]
	++ ["</table>"])

buildToC :: [Section] -> String
buildToC secs = "<div id='toc'>" ++ intercalate " | " [
	"<a href='#" ++ toKey heading ++ "'>" ++ heading ++ "</a>"
	| (heading, _) <- secs] ++ "</div>"

startHTML = unlines [
	"<html>",
	"<head>",
	"<title>Hyperpolymorph</title>",
	"<meta charset='utf-8' />",
	"<link rel='stylesheet' href='morph.css' />",
	"</head>",
	"<body>",
	"<div id='wrapper'>",
	"<h1>Hyperpolymorph</h1>",
	"<p id='cc'>Based on / inspired by <a href='http://hyperpolyglot.org/'>Hyperpolyglot.org</a>; released under <a href='http://creativecommons.org/licenses/by-sa/3.0/'>CC BY-SA 3.0</a>. Work in progress!</p>",
	"<a href='https://github.com/betaveros/hyperpolymorph'><img style='position: absolute; top: 0; right: 0; border: 0;' src='https://camo.githubusercontent.com/365986a132ccd6a44c23a9169022c0b5c890c387/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f7265645f6161303030302e706e67' alt='Fork me on GitHub' data-canonical-src='https://s3.amazonaws.com/github/ribbons/forkme_right_red_aa0000.png'></a>",
	"<div id='morpher'></div>"
	]
footHTML = "</div>"
endHTML = unlines ["</body>", "</html>"]

buildJsCall names skeys = unlines [
	"<script type='text/javascript' src='morph.js'></script>",
	"<script type='text/javascript'>",
	"makeMorpher([" ++ intercalate "," ["\"" ++ name ++ "\"" | name <- names] ++ "]);",
	"makeToCScroller([" ++ intercalate "," ["\"" ++ skey ++ "\"" | skey <- skeys] ++ "]);",
	"</script>"
	]

parseIO parser msg txt = either (ioError . userError . show) return $ parse parser msg txt

readEntryMap name = readFile (name ++ ".txt") >>= parseIO bqEntryMap name
readNamedEntryMap name = (,) name <$> readEntryMap name

-- langNames = ["perl","php","python","ruby","tcl","lua","javascript","groovy","cpp","objective-c","java","c-sharp","c","go","pascal","ada","plpgsql","common-lisp","racket","clojure","c-sharp","ocaml","f-sharp","scala","haskell","prolog","erlang","forth","postscript","factor","posix-shell","applescript","powershell","sql","awk","pig","matlab","r","numpy","fortran","mathematica","sympy","maxima","pari-gp"]
langNames = ["ocaml","haskell","coffeescript","javascript","perl","php","python","cpp","java","scala"]


main = do
	cont <- readFile "entries.txt"
	secs <- parseIO bqSectionList "(entries.txt)" cont
	langs <- mapM readNamedEntryMap langNames
	putStrLn startHTML
	putStrLn $ buildTable secs langs
	putStrLn footHTML
	putStrLn $ buildToC secs
	putStrLn . buildJsCall langNames $ map (toKey . fst) secs
	putStrLn endHTML
