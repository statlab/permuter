# permuter

R functions to develop the application examples presented in "Permutation Tests
for Complex Data: Theory, Applications and Software" - Pesarin, Salmaso

http://www.wiley.com/legacy/wileychi/pesarin/material.html

## Install
To install `permuter`, run the following line in R:

```
devtools::install_github("statlab/permuter")
```

## Development process

The development process will follow the conventions in [R Packages](http://r-pkgs.had.co.nz).


* You will need the following packages: `devtools`, `roxygen2`, and `testthat`.

* Load in the functions and data using `devtools::load_all()` or cmd+shift+L in RStudio.

* When you've updated documentation, run `devtools::document()` or cmd+shift+D in RStudio to compile it.

* Write tests for all functions and run `devtools::test()` or cmd+shift+T in RStudio to run all the tests.

* Cmd+shift+B in RStudio restarts the session and rebuilds the R package, running all tests in the process.

