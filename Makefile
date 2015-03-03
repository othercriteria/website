# Broadly inspired (copied at the outset) by Makefile at:
#   https://github.com/KirinDave/public-website/blob/master/Makefile

RMD_SRC = $(wildcard rmd_stage/*.Rmd)
RMD_OUT = ${RMD_SRC:.Rmd=.md}
SUFFIXES = .Rmd .md

site: _site

Main: site.hs
	ghc --make site -optl -w -dynamic -O2

_site: Main $(RMD_OUT) css/*.css posts/* images/* links/* root/* root_static/*
	./site rebuild

preview: _site
	./site watch

sync: _site
	s3cmd -P sync _site/ s3://mesokurtosis.com/

analytics:
	s3cmd sync s3://logs.mesokurtosis.com/root/ logs/
	./analytics.py

stage:
	rsync -avzh --exclude '.DS_Store' --exclude '*.Rmd' \
	  ~/Dropbox/mesokurtosis/* .
	rsync -avzh ~/Dropbox/mesokurtosis/posts/*.Rmd rmd_stage/

%.md : %.Rmd
	Rscript \
	  -e "library(knitr)" \
	  -e "opts_chunk[['set']](fig.path='$(patsubst %.md,%,$(@F))-')" \
	  -e "opts_knit[['set']](base.dir='images/')" \
	  -e "opts_knit[['set']](base.url='../images/')" \
	  -e "knit('$<', output='$(@F)')"
	mv $(@F) posts/

cache_backup:
	rsync -avzh cache/* ~/Dropbox/mesokurtosis/cache/

cache_restore:
	rsync -avzh ~/Dropbox/mesokurtosis/cache/* cache/

unstage:
	rm -rf posts/
	rm -rf images/
	rm -rf links/
	rm -rf rmd_stage/
	rm -rf cache/

clean: unstage
	find . -name '*~' | xargs rm
	find . -name '#*#' | xargs rm
	find . -name '*.o' | xargs rm
	rm site.hi
