R_OPTS = --vanilla

vignettes := $(wildcard vignettes/*.Rmd)
vignette_html := $(vignettes:.Rmd=.html)
	
.PHONY: all tidy

all: tidy $(vignette_html)
			
tidy:
	cd vignettes; python3 tidy_Rdm.py
	# add a line here to tidy all the R code and tests

.DELETE_ON_ERROR:
vignettes/%.html: vignettes/%.Rmd
	Rscript $(R_OPTS) -e "rmarkdown::render('$<')"

vignettes: $(vignette_html)