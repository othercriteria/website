# Broadly inspired (copied at the outset) by Makefile at:
#   https://github.com/KirinDave/public-website/blob/master/Makefile

site: _site

Main: site.hs
	ghc --make site -optl -w

_site: Main css/*.css posts/*
	./site rebuild

preview: _site
	./site watch

sync: _site
	s3cmd -P sync _site/ s3://mesokurtosis.com/

analytics:
	s3cmd sync s3://logs.mesokurtosis.com/root/ logs/

stage:
	cp -r ~/Dropbox/mesokurtosis/posts .
	cp -r ~/Dropbox/mesokurtosis/images .
	cp -r ~/Dropbox/mesokurtosis/links .

unstage:
	rm -rf posts/
	rm -rf images/
	rm -rf links/

clean: unstage
	find . -name '*~' | xargs rm
	find . -name '#*#' | xargs rm
	find . -name '*.o' | xargs rm
	rm site.hi