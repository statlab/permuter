# Python 3 script to tidy format for vignettes in
#  https://github.com/statlab/permuter

from fileinput import FileInput
import subprocess

from rpy2.robjects import r as R

files = ('autofluorescence.Rmd',
         'botulinum.Rmd',
         'chrom17m.Rmd',
         'confocal.Rmd',
         'examples_chapters1_4.Rmd',
         'germina.Rmd',
         'kenya.Rmd',
         'massaro_blair.Rmd',
         'monachus.Rmd',
         'perc.Rmd',
         'rats.Rmd',
         'setig.Rmd',
         'three_sample_surv.Rmd',
         'two_sample_surv.Rmd',
         'urology.Rmd',
         'washing.Rmd',
         'waterfalls.Rmd',
         'westfall_wolfinger.Rmd')


R("library(formatR)")

tidy = R('''
tidy <- function(snippet) {
    tidy_source(text=snippet, arrow=TRUE, width.cutoff=79)$text.tidy
}''')


with FileInput(files=files, inplace=1) as fh:
    for line in fh:
        print(line, end="")
        if line.startswith('```{r'):
            snippet = ""
            for line in fh:
                if line.startswith('```'):
                    tidy(snippet)
                    print(line, end="")
                    break
                else:
                    snippet += line