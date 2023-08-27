.PHONY: build

build:
	rm ais.cabal && stack build

site-build:
	stack exec site build

watch:
	stack exec site watch

deploy:
	git subtree push --prefix _site origin gh-pages
