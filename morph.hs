import qualified Data.Map as M
import Control.Arrow
import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- utility HTML-making functions

makeTag tagname cls cont =
	"<" ++ tagname ++ " class='" ++ cls ++ "'>" ++ cont ++ "</" ++ tagname ++ ">"

makeSpanTag = makeTag "span"
makeStrongTag = makeTag "strong"

-- parsing

isID c = isAlphaNum c || c == '_'

type CharParser = Parsec [Char] ()

bqMarkup :: CharParser String
bqMarkup = many bqAtom >>= return . concat

bqAtom :: CharParser String
bqAtom = bqSpecial <|> plainAtom

plainAtom :: CharParser String
plainAtom = htmlEscaped <|> many1 (satisfy isID) <|> fmap return anyChar

htmlEscaped :: CharParser String
htmlEscaped = esc '&' "&amp;" <|> esc '<' "&lt;" <|> esc '>' "&gt;" <|> esc '\n' "<br />"
	where esc c s = char c >> return s

bqSpecial :: CharParser String
bqSpecial = char '`' >>
	((char '`' >> return "`")
	<|> (char '[' >> return "[")
	<|> (char ']' >> return "]")
	<|> (char '!' >> fmap (makeSpanTag "key") bqGroup)
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

startHTML = unlines [
	"<html>",
	"<head>",
	"<title>Hyperpolymorph</title>",
	"<link rel='stylesheet' href='hyperpolymorph.css' />",
	"</head>",
	"<body>"
	]
endHTML = unlines ["</body>", "</html>"]
