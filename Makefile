index.html : morph
	./morph ruby python javascript cpp d java ocaml haskell coffeescript perl php scala > index.html

morph : morph.hs
	ghc --make morph.hs

clean :
	rm index.html morph
