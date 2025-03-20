
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ROCnGO app

<!-- badges: start -->

<!-- badges: end -->

[ROCnGO](https://github.com/pabloPNC/ROCnGO) package implementation in a
Shiny application. The implementation provides an user interface to use
library main functions.

## Installation and Requirements

Latest app version can be downloaded from its
[GitHub](https://github.com/) repository, as any other package. Since
application also depends on ROCnGO, the library should be also be
installed.

ROCnGO installation steps can be found on its
[repository](https://github.com/pabloPNC/ROCnGO). Alternatively, both
the library and application can be installed by using the following
commands:

``` r
# install.packages("devtools")
# install ROCnGO dependency if not installed yet
if(!require("ROCnGO")) {
  devtools::install_github("pabloPNC/ROCnGO")
}
# install rocngo-app
devtools::install_github("pabloPNC/rocngo-app")
```

## Usage

Type following command to launch app:

``` r
roc_app()
```
