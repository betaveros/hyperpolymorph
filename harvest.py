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
}
def normalize(e):
	h = re.sub(r'\W+', '-', e.lower()).strip('-')
	return overrides.get(h, h)
	

def parse_entries(filename, outfilename):
	with codecs.open(filename, encoding="utf-8") as infile:
		with codecs.open(outfilename, 'w', encoding="utf-8") as outfile:
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

parse_entries('scripting.html', 'entries.txt')
parse('scripting.html', ['perl.txt', 'php.txt', 'python.txt', 'ruby.txt'])
parse('more.html', ['tcl.txt', 'lua.txt', 'javascript.txt', 'groovy.txt'])
parse('cpp.html', ['cpp.txt', 'objective-c.txt', 'java.txt', 'c-sharp.txt'])
parse('lisp.html', ['common-lisp.txt', 'racket.txt', 'java.txt', 'c-sharp.txt'])
parse('ml.html', ['ocaml.txt', 'f-sharp.txt', 'scala.txt', 'haskell.txt'])
