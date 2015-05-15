# permuter

R functions to develop the application examples presented in "Permutation Tests
for Complex Data: Theory, Applications and Software" - Pesarin, Salmaso

http://www.wiley.com/legacy/wileychi/pesarin/material.html


## Development process

The development process will follow the conventions in [R Packages](http://r-pkgs.had.co.nz).

* R_functions/ contains the original files. R/ contains the versions for development.

* You will need the following packages: `devtools`, `roxygen2`, and `testthat`.

* Load in the functions and data using `devtools::load_all()` or cmd+shift+L in RStudio.

* When you've updated documentation, run `devtools::document()` or cmd+shift+D in RStudio to compile it.

* Cmd+shift+B in RStudio restarts the session and rebuilds the R package, running all tests in the process.

