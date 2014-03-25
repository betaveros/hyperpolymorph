from __future__ import print_function
from bs4 import BeautifulSoup, NavigableString
import sys, codecs, re


def textish(node):
	for d in node.descendants:
		if isinstance(d, NavigableString):
			yield unicode(d)
		elif d.name == "br":
			yield "\n"



overrides = {
	"statement-separator": "statement-terminator",
	"write-once-variable": "constant",
	"arithmetic-operators-addition-subtraction-multiplication-float-division-quotient-remainder": "arithmetic-operators",
	"compound-assignment-arithmetic-bit": "compound-assignment-arithmetic-string-logical-bit",
	"transcendental-constants-and-e": "transcendental-constants",
	"float-truncation-towards-zero-to-nearest-integer-towards-towards": "float-truncation",
	"random-seed-set-get-restore": "random-seed",
	"translate-case-to-upper-to-lower": "translate-case",
	"format-string": "format",
	"concatenate-and-append": "concatenate",
	"trim-both-sides-left-right": "trim",
	"pad-on-left-on-right-centered": "pad",
	"pad-on-right-on-left": "pad",
	"length": "string-length",
	"index-of-substring-first-last": "index-of-substring",
	"extract-substring-by-start-and-length-by-start-and-end-by-successive-starts": "extract-substring",
	"date-and-time-type": "date-time-type",
	"current-date-and-time": "current-date-time",
	"result-of-date-subtraction": "date-subtraction",
	"add-time-duration": "add-duration",
	"date-parts": "date-parts",
	"time-parts": "time-parts",
	"element-index": "index-of-element",
	"slice-by-endpoints-by-length": "slice",
	"replicate-element": "replicate", # clash?
	"copy-address-copy-shallow-copy-deep-copy": "copy",
	"reverse-non-destructive-in-place": "reverse",
	"sort-non-destructive-in-place-custom-comparision": "sort",
	"dedupe-non-destructive-in-place": "dedupe",
	"finally-ensure": "finally",
	"finally-clause": "finally",
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
					f.write('`e ')
					f.write(e)
					f.write('\n')
				for td, f in zip(tds[1:], colfiles):
					for txt in textish(td):
						f.write(txt.replace('`','``').replace(u'\xa0', u' '))
					f.write('\n')
	for f in colfiles: f.close()

# to_parse_entries = [('scripting.html', 'entries.txt', False),('cpp.html', 'entries.txt'),('c.html', 'entries.txt'),('pascal.html', 'entries.txt'),('lisp.html', 'entries.txt'),('ml.html', 'entries.txt')]
# to_parse_entries = [('scripting.html', 'entries.txt', False),('cpp.html', 'entries.cpp.txt', False)]
to_parse_entries = []
to_parse = [('scripting.html', ['perl.txt', 'php.txt', 'python.txt', 'ruby.txt']),('more.html', ['tcl.txt', 'lua.txt', 'javascript.txt', 'groovy.txt']),('cpp.html', ['cpp.txt', 'objective-c.txt', 'java.txt', 'c-sharp.txt']),('c.html', ['c.txt', 'go.txt']),('pascal.html', ['pascal.txt', 'ada.txt', 'plpgsql.txt']),('lisp.html', ['common-lisp.txt', 'racket.txt', 'clojure.txt', 'c-sharp.txt']),('ml.html', ['ocaml.txt', 'f-sharp.txt', 'scala.txt', 'haskell.txt']),('logic.html', ['prolog.txt', 'erlang.txt']),('stack.html', ['forth.txt', 'postscript.txt', 'factor.txt']),('shell.html', ['posix-shell.txt', 'applescript.txt', 'powershell.txt']),('data.html', ['sql.txt', 'awk.txt', 'pig.txt']),('numerical-analysis.html', ['matlab.txt', 'r.txt', 'numpy.txt']),('fortran.html', ['fortran.txt']),('computer-algebra.html', ['mathematica.txt', 'sympy.txt', 'maxima.txt', 'pari-gp.txt']),]

for ix, t in enumerate(to_parse_entries):
	print("{}/{} parsing entries {}...".format(ix + 1, len(to_parse_entries), t[0]), end="")
	parse_entries(*t)
	print(" OK")

print()

for ix, t in enumerate(to_parse):
	print("{}/{} parsing {}...".format(ix + 1, len(to_parse), t[0]), end="")
	parse(*t)
	print(" OK")
