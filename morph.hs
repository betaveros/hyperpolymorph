import qualified Data.Map as M
import Control.Arrow
import Control.Monad
import Data.Char
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

type CharParser = Parsec [Char] ()

bqMarkup :: CharParser String
bqMarkup = many bqAtom >>= return . concat

bqAtom :: CharParser String
bqAtom = bqSpecial <|> plainAtom

plainAtom :: CharParser String
plainAtom = htmlEscaped <|> many1 idChar <|> fmap return (noneOf "`")

htmlEscaped :: CharParser String
htmlEscaped = esc '&' "&amp;" <|> esc '<' "&lt;" <|> esc '>' "&gt;" <|> esc '\n' "<br />"
	where esc c s = char c >> return s

bqSpecial :: CharParser String
bqSpecial = try $ char '`' >>
	((char '`' >> return "`")
	<|> (char '[' >> return "[")
	<|> (char ']' >> return "]")
	<|> (char '*' >> fmap makeStrongTag bqGroup)
	<|> (char '!' >> fmap (makeSpanTag "key") bqGroup)
	<|> (char '.' >> fmap (makeSpanTag "builtin") bqGroup)
	<|> (char '=' >> fmap (makeSpanTag "val") bqGroup)
	<|> (fmap (makeSpanTag "aside") $ plainMatching '(' ')')
	)

bqMatching :: Char -> Char -> CharParser String
bqMatching lc rc = do
	char lc
	ss <- manyTill (bqMatching lc rc <|> bqAtom) (char rc)
	return $ concat ss

plainMatching :: Char -> Char -> CharParser String
plainMatching lc rc = do
	char lc
	ss <- manyTill (plainMatching lc rc <|> plainAtom) (char rc)
	return $ concat ss

bqGroup :: CharParser String
bqGroup = (bqMatching '[' ']') <|> bqAtom

parseBqMarkup input = parse bqMarkup "(unknown)" input

bqEntry :: CharParser (String, String)
bqEntry = do
	string "`e"
	many1 (oneOf " \t")
	name <- many1 idChar
	many1 space
	cont <- bqMarkup
	return (name, cont)

type EntryMap = M.Map String String

bqEntryMap :: CharParser EntryMap
bqEntryMap = do
	entries <- many bqEntry
	return $ M.fromList entries

-- parse the entry associations used to create final output

bqEntryAssoc :: CharParser (String, String)
bqEntryAssoc = do
	key <- many1 idChar
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

-- make everything together!

buildTd :: String -> EntryMap -> String
buildTd key emap = case M.lookup key emap of
	Nothing -> "<td class='empty'></td>"
	Just s -> "<td>" ++ s ++ "</td>"

buildRow :: (String, String) -> [EntryMap] -> String
buildRow (key, desc) emaps =
	"<tr><td class='desc'>" ++ desc ++ "</td>" ++ concatMap (buildTd key) emaps ++ "</tr>"

makeThRow colspan cont =
	"<tr><th colspan='" ++ show colspan ++ "'>" ++ cont ++ "</th></tr>"

buildSection :: Section -> [EntryMap] -> String
buildSection (heading, assocs) emaps = unlines (
	makeThRow (length emaps + 1) heading : [
		buildRow assoc emaps | assoc <- assocs
	])

buildTable :: [Section] -> [EntryMap] -> String
buildTable secs emaps = unlines (["<table>"]
	++ [buildSection sec emaps | sec <- secs]
	++ ["</table>"])

startHTML = unlines [
	"<html>",
	"<head>",
	"<title>Hyperpolymorph</title>",
	"<link rel='stylesheet' href='morph.css' />",
	"</head>",
	"<body>"
	]
endHTML = unlines ["</body>", "</html>"]

parseIO parser txt = either (ioError . userError . show) return $ parse parser "???" txt

main = do
	cont <- readFile "entries.txt"
	secs <- parseIO bqSectionList cont
	pycont <- readFile "python.txt"
	pymap <- parseIO bqEntryMap pycont
	putStrLn startHTML
	putStrLn $ buildTable secs [pymap]
	putStrLn endHTML
