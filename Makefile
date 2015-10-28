index.html : morph
	./morph entries +python ruby javascript coffeescript perl php cpp +d java +scala ocaml +haskell lua > index.html

morph : morph.hs
	ghc --make morph.hs

clean :
	rm index.html morph
