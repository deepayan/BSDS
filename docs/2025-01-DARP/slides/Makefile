
all: 01-introduction.html 02-basics.html 03-lists-data-frames.html \
     04-functions-attributes.html 05-lazy-evaluation.html \
     06-visualization-basics.html 07-ggplot.html

%.md: %.rmd
	Rscript -e "library(knitr); knit('$<')"

%.R: %.rmd
	Rscript -e "library(knitr); purl('$<', documentation = 0)"

%.html: %.md
	## cp '$<' '$@'
	Rscript -e "source('rmd2remark.R'); remarkize('$<')"

