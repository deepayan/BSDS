
all: 01-introduction.html 02-basics.html 03-lists-data-frames.html

%.md: %.rmd
	Rscript -e "library(knitr); knit('$<')"

%.R: %.rmd
	Rscript -e "library(knitr); purl('$<', documentation = 0)"

%.html: %.md
	## cp '$<' '$@'
	Rscript -e "source('rmd2remark.R'); remarkize('$<')"

