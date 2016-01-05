# Broadly inspired (copied at the outset) by Makefile at:
#   https://github.com/KirinDave/public-website/blob/master/Makefile


RMD_SRC := $(wildcard rmd_stage/*.Rmd)
RMD_OUT := $(RMD_SRC:.Rmd=.md)
.SUFFIXES:
.SUFFIXES: .Rmd .md

generate: _site

Main: generator/Main.hs
	stack build
	ln -s -f .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/website/website site

_site: Main $(RMD_OUT) posts/* css/*.css images/* links/* root/* root_static/*
	./site rebuild

preview: _site
	./site watch

sync: _site
	./site deploy

analytics:
	s3cmd sync --delete-after-fetch s3://logs.mesokurtosis.com/root/ logs/
	rsync -avzh logs/ ~/Dropbox/mesokurtosis/logs/
	mkdir -p analytics_out
	./analytics.py
	cp analytics_out/* images/
	echo `date` > root/analytics_date

stage:
	rsync -avzh \
	  --exclude '.DS_Store' --exclude '*.Rmd' --exclude '*.prelim' \
	  --exclude 'logs/*' \
	  ~/Dropbox/mesokurtosis/* .
	rsync -avzh ~/Dropbox/mesokurtosis/posts/*.Rmd rmd_stage/

%.md: %.Rmd
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
	rm -rf logs/
	rm -rf posts/
	rm -rf images/
	rm -rf links/
	rm -rf trivia/
	rm -rf rmd_stage/
	rm -rf cache/
	rm -rf analytics_out/

clean: unstage
	rm -rf _cache/
	rm -rf _site/
	find . -name '*~' | xargs rm
	find . -name '#*#' | xargs rm
