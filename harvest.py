from __future__ import print_function
from bs4 import BeautifulSoup, NavigableString
import sys, codecs, re

def textish(node):
	for d in node.descendants:
		if isinstance(d, NavigableString):
			yield unicode(d).replace("\n", "")
		elif d.name == "br":
			yield "\n"

def get_surroundings(style):
	if "purple" in style: return "`*"
	if "teal" in style: return "`."
	if "maroon" in style: return "`="
	if "gray" in style: return "`c"
	return None

def marked_text(node):
	for d in node.children:
		if isinstance(d, NavigableString):
			yield unicode(d).replace("\n", "").replace("`","``")
		elif d.name == "br":
			yield "\n"
		else:
			try:
				sur = get_surroundings(d["style"])
			except KeyError:
				sur = None
			if sur: yield sur
			needBracket = False
			buf = []
			for t in marked_text(d):
				if sur and not needBracket and all(c.isalnum() or c == '_' for c in t):
					buf.append(t)
				else:
					if sur and not needBracket:
						needBracket = True
						yield "["
						for tt in buf: yield tt
					yield t
			for tt in buf: yield tt
			if sur and needBracket: yield "]"

overrides = {
	"statement-separator": "statement-terminator",
	"blocks": "block-delimiters",
	"modifiable-variable": "local-variable",
	"write-once-variable": "constant",
	"arithmetic-operators-addition-subtraction-multiplication-float-division-quotient-remainder": "arithmetic-operators",
	"compound-assignment-arithmetic-bit": "compound-assignment-arithmetic-string-logical-bit",
	"integer-division-and-remainder": "divmod",
	"transcendental-constants-and-e": "transcendental-constants",
	"float-truncation-towards-zero-to-nearest-integer-towards-towards": "float-truncation",
	"float-truncation-round-towards-zero-round-to-nearest-integer-round-down-round-up": "float-truncation",
	"random-number-uniform-int-uniform-float-normal-float": "random-number-uniform-integer-uniform-float-normal-float",
	"random-seed-set-get-restore": "random-seed",
	"random-seed-how-to-set": "random-seed",
	"bit-operators-left-shift-right-shift-and-inclusive-or-exclusive-or-complement": "bit-operators",
	"radix": "radix-convert-integer-to-and-from-string-with-radix",
	"character-escapes": "literal-escapes",
	"translate-case-to-upper-to-lower": "translate-case",
	"case-manipulation": "translate-case",
	"format-string": "format",
	"string-concatenation": "concatenate-strings",
	"trim-both-sides-left-right": "trim",
	"pad-on-left-on-right": "pad",
	"pad-on-left-on-right-centered": "pad",
	"pad-on-right-on-left-centered": "pad",
	"pad-on-right-on-left": "pad",
	"length": "string-length",
	"index-of-substring-first-last": "index-of-substring",
	"extract-substring-by-start-and-length-by-start-and-end-by-successive-starts": "extract-substring",
	"date-and-time-type": "date-time-type",
	"date-and-time-types": "date-time-type",
	"current-date-and-time": "current-date-time",
	"result-of-date-subtraction": "date-subtraction",
	"add-time-duration": "add-duration",
	"date-parts": "date-parts",
	"time-parts": "time-parts",
	"empty-list-test": "empty-test",
	"nth-element": "list-lookup",
	"element-index": "index-of-element",
	"shuffle": "shuffle-and-sample",
	"index-of-element-first-and-last-occurrence": "index-of-element",
	"slice-by-endpoints-by-length": "slice",
	"backreference-in-regex-in-substitution-string": "backreference-in-match-and-substitution",
	"get-date-parts": "date-parts",
	"get-time-parts": "time-parts",
	"dict-literal": "map-literal",
	"dict-size": "map-size",
	"replicate-element": "replicate",
	"relative-complement": "relative-complement-symmetric-difference",
	"function-declaration": "declare-function",
	"function-invocation": "invoke-function",
	"named-parameter": "named-parameters",
	"missing-argument-value": "missing-argument-behavior",
	"if-else-if-else": "control-structure-keywords",
	"break-and-continue": "break-continue-redo",
	"copy-address-copy-shallow-copy-deep-copy": "copy",
	"sort-non-destructive-in-place-custom-comparision": "sort",
	"dedupe-non-destructive-in-place": "dedupe",
	"finally-ensure": "finally",
	"finally-clause": "finally",
	"namespace-declaration": "declare-namespace",
	"write-formatted-string-to-stdout": "printf",
	"file-test-regular-file-test": "file-exists-test-regular-file-test",
	"temporary-directory": "system-temporary-file-directory",
	"class-definition": "define-class",
	"object-creation": "create-object",
	"method-invocation": "invoke-method",
	"typedef": "type-synonym",
}
def normalize(e):
	h = re.sub(r'\W+', '-', e.lower()).strip('-')
	return overrides.get(h, h)

def parse_entries(filename, outfilename, append=True):
	with codecs.open(filename, 'r', encoding="utf-8") as infile:
		with codecs.open(outfilename, 'a' if append else 'w', encoding="utf-8") as outfile:
			outfile.write('# general\n')
			soup = BeautifulSoup(infile)
			t = soup.table
			for row in t.find_all('tr'):
				ths = row.find_all('th')
				if ths and ths[0].get('colspan'):
					outfile.write('# ')
					outfile.write(''.join(textish(ths[0])).strip().replace('\n', ' / '))
					outfile.write('\n')
				tds = row.find_all('td')
				if tds:
					name = ''.join(textish(tds[0])).strip().replace('\n', ' / ')
					e = normalize(name)
					outfile.write(e)
					outfile.write(': ')
					outfile.write(name)
					outfile.write('\n')

def parse(filename, colfilenames):
	colfiles = [codecs.open(fn, 'w', encoding="utf-8") for fn in colfilenames]

	with codecs.open(filename, encoding="utf-8") as infile:
		soup = BeautifulSoup(infile)
		t = soup.table
		for row in t.find_all('tr'):
			tds = row.find_all('td')
			if tds:
				for f in colfiles:
					name = ''.join(textish(tds[0]))
					e = normalize(name)
					if e:
						f.write('`e ')
						f.write(e)
						f.write('\n')
				for td, f in zip(tds[1:], colfiles):
					for txt in marked_text(td):
						f.write(txt.replace(u'\xa0', u' '))
					f.write('\n')
	for f in colfiles: f.close()

# to_parse_entries = [('scripting.html', 'entries.txt', False),('cpp.html', 'entries.txt'),('c.html', 'entries.txt'),('pascal.html', 'entries.txt'),('lisp.html', 'entries.txt'),('ml.html', 'entries.txt')]
# to_parse_entries = [('ml.html', 'entries.ml.txt', False)]
# to_parse_entries = [('scripting2.html', 'scripting2.txt')]
to_parse_entries = []
# to_parse = [('scripting.html', ['perl.txt', 'php.txt', 'python.txt', 'ruby.txt']),('more.html', ['tcl.txt', 'lua.txt', 'javascript.txt', 'groovy.txt']),('cpp.html', ['cpp.txt', 'objective-c.txt', 'java.txt', 'c-sharp.txt']),('c.html', ['c.txt', 'go.txt']),('pascal.html', ['pascal.txt', 'ada.txt', 'plpgsql.txt']),('lisp.html', ['common-lisp.txt', 'racket.txt', 'clojure.txt', 'emacs-lisp.txt']),('ml.html', ['ocaml.txt', 'f-sharp.txt', 'scala.txt', 'haskell.txt']),('logic.html', ['prolog.txt', 'erlang.txt']),('stack.html', ['forth.txt', 'postscript.txt', 'factor.txt']),('shell.html', ['posix-shell.txt', 'applescript.txt', 'powershell.txt']),('data.html', ['sql.txt', 'awk.txt', 'pig.txt']),('numerical-analysis.html', ['matlab.txt', 'r.txt', 'numpy.txt']),('fortran.html', ['fortran.txt']),('computer-algebra.html', ['mathematica.txt', 'sympy.txt', 'maxima.txt', 'pari-gp.txt']),]
# to_parse = [('ml.html', ['sml-new.txt', 'ocaml-new.txt', 'f-sharp-new.txt', 'haskell-new.txt'])]
# to_parse = [('scripting2.html', ['perl2.txt', 'php2.txt', 'python2.txt', 'ruby2.txt'])]
# to_parse = [('rust.html', ['rust.txt', 'swift.txt', 'scala2.txt'])]
to_parse = [('scripting.html', ['javascript-new.txt', 'php-new.txt', 'python-new.txt', 'ruby-new.txt'])]
# to_parse = [('more.html', ['tcl.txt', 'lua.txt', 'javascript.txt', 'groovy.txt'])]
# to_parse = [('lisp.html', ['common-lisp.txt', 'racket.txt', 'clojure.txt', 'emacs-lisp.txt'])]

for ix, t in enumerate(to_parse_entries):
	print("{}/{} parsing entries {}...".format(ix + 1, len(to_parse_entries), t[0]), end="")
	parse_entries(*t)
	print(" OK")

print()

for ix, t in enumerate(to_parse):
	print("{}/{} parsing {}...".format(ix + 1, len(to_parse), t[0]), end="")
	parse(*t)
	print(" OK")
