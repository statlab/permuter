files <- list.files()
library(formatR)
for (f in files) {
    tidy_source(source = f, file = f, arrow = TRUE, width.cutoff = 79)
}

setwd("../tests/testthat")
files <- list.files()
for (f in files) {
    tidy_source(source = f, file = f, arrow = TRUE, width.cutoff = 79)
} 
