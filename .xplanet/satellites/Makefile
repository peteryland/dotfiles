#!/bin/make -f

default: all config

clean:
	@rm -f all config

dist-clean: clean
	@rm -f all.tle

distclean: dist-clean

all.tle:
	@wget 'https://celestrak.org/NORAD/elements/gp.php?GROUP=active&FORMAT=tle' -q -T 5 --no-cache -O all.txt > /dev/null 2>&1
	@tr -d '' < all.txt > all.tle
	@rm -f all.txt

all: all.tle Makefile
	@while read tlename; do read dummy; read dummy SAT dummy; echo "$${SAT} {} opacity=0.2 transparent={0,0,0} color={100,100,100} symbolsize=0 fontsize=2"; done <all.tle >all

config: all Makefile
	@echo "satellite_file=satellites/all" > config
	@echo "$(date +%Y%m%d-%h%m)" > .last_updated

.PHONY: default clean dist-clean distclean
