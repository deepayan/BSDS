
all: 01-introduction.html 02-distribution.html 03-data-summary.html

%.md: %.rmd
	Rscript -e "library(knitr); knit('$<')"

%.R: %.rmd
	Rscript -e "library(knitr); purl('$<', documentation = 0)"

%.html: %.md
	## cp '$<' '$@'
	Rscript -e "source('rmd2remark.R'); remarkize('$<')"

