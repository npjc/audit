
<!-- README.md is generated from README.Rmd. Please edit that file -->

# audit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

An interactive application that ushers you through the inspection and
analysis of your growth curve experiments.

## Example

![alt text](eg.gif)

To start a new AUDIT session:

``` r
audit::run()
```

## Installation

Audit is currently only available on github and can be installed like
any other remote r package:

``` r
remotes::install_github("npjc/audit")
```

#### Step-by-step

1.  Install [R](https://cran.r-project.org) by downloading the
    appropriate package for your operating system from
    [cran](https://cran.r-project.org) and following the onscreen
    instructions.
2.  Install [Rstudio](https://www.rstudio.com/products/rstudio)
    (Optional), a development environment for R by downloading the
    appropriate image for your operating system from Rstudio’s [download
    page](https://www.rstudio.com/products/rstudio/download/#download)
    and follow the onscreen instructions.
3.  If this is a new R installation you will need to download some
    packages before being able to install audit. To do this, open R /
    Rstudio and enter the line below into the console. Follow the
    onscreen instructions.

<!-- end list -->

``` r
install.packages(c("devtools", "tidyverse", "remotes"))
```

4.  Install AUDIT and its dependencies by entering the line below into
    the console

<!-- end list -->

``` r
remotes::install_github("npjc/audit")
```

5.  To run AUDIT enter the line below at the console. This will open a
    new session in your default web browser.

<!-- end list -->

``` r
audit::run()
```
