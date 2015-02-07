# Broadly inspired (copied at the outset) by Makefile at:
#   https://github.com/KirinDave/public-website/blob/master/Makefile

site: _site

Main: Main.hs
	ghc --make Main -optl -w

_site: Main css/*.css posts/*
	./Main rebuild

preview: _site
	./Main preview

sync: _site
	s3cmd -P sync _site/ s3://www.mesokurtosis.com/

stage:
	cp -r ~/Dropbox/mesokurtosis/posts .
	cp -r ~/Dropbox/mesokurtosis/images .

unstage:
	rm -rf posts/
	rm -rf images/

clean: unstage
	find . -name '*~' | xargs rm
	find . -name '#*#' | xargs rm
	find . -name '*.o' | xargs rm
	rm Main.hi