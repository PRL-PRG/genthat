genthat - test case generation for R
=====

<!-- badges: start -->
[![R-CMD-check](https://github.com/PRL-PRG/genthat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PRL-PRG/genthat/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/PRL-PRG/genthat/branch/master/graph/badge.svg)](https://codecov.io/gh/PRL-PRG/genthat)
[![mutator](https://img.shields.io/endpoint?url=https%3A%2F%2Fprl-prg.github.io%2Fgenthat%2Fmutation-score.json)](https://github.com/PRL-PRG/genthat/actions/workflows/mutation-testing.yaml)
<!-- badges: end -->

*genthat* is a framework for unit tests generation from source code and for test execution, and filtering of test cases based on C code coverage using `gcov` and R code coverage using `covr`.

# Installation

Even thought the development of the package started sometime ago it is still
rather experimental and no available from CRAN release yet.
However, that is one of the near future plans to have a stable version and
release it through CRAN.

It can be installed easily using the `devtools` package:

```r
library(devtools)
install_github('PRL-PRG/genthat')
```

Or download the sources and build manually.

Usage
-----

Look for the [genthat overview](https://htmlpreview.github.io/?https://raw.githubusercontent.com/fikovnik/ISSTA18-artifact/master/overview.html)

More resources:
- [ISSTA'18 paper](http://janvitek.org/pubs/issta18.pdf)
- [ISSTA'18 artifact](https://github.com/fikovnik/ISSTA18-artifact)
