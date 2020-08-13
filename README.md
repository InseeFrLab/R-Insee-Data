insee R package
================

<br>

[![CRAN status](https://www.r-pkg.org/badges/version/insee)](https://cran.r-project.org/package=insee)
[![CRAN checks](https://cranchecks.info/badges/worst/insee)](https://cran.r-project.org/web/checks/check_results_insee.html)
[![Codecov test coverage](https://codecov.io/gh/hadrilec/insee/branch/master/graph/badge.svg)](https://codecov.io/gh/hadrilec/insee?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/insee)](https://cran.r-project.org/package=insee)
[![Downloads](https://cranlogs.r-pkg.org/badges/insee)](https://cran.r-project.org/package=insee)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)

<br>

## Overview

The insee package contains tools to easily download data and metadata
from INSEE main database (BDM).

Using embedded SDMX queries, get the data of more than 140 000 INSEE
series.

## Installation & Loading

``` r
# Get the development version from GitHub
# install.packages("devtools")
# devtools::install_github("hadrilec/insee")

# Get the CRAN version
install.packages("insee")

# examples below use tidyverse packages 
library(tidyverse)
library(insee)
```

## Examples & Tutorial

  - [Tutorial](https://hadrilec.github.io/insee/articles/1_insee-vignettes.html)
  - [GDP growth
    rate](https://hadrilec.github.io/insee/articles/2_gdp-vignettes.html)
  - [Inflation](https://hadrilec.github.io/insee/articles/3_inflation-vignettes.html)
  - [Unemployment
    rate](https://hadrilec.github.io/insee/articles/4_unem-vignettes.html)
  - [Population by
    age](https://hadrilec.github.io/insee/articles/5_pop-vignettes.html)
  - [Population
    map](https://hadrilec.github.io/insee/articles/6_pop_map-vignettes.html)

## How to avoid proxy issues ?

``` r
Sys.setenv(http_proxy = "my_proxy_server")
Sys.setenv(https_proxy = "my_proxy_server")
```

## Support

Feel free to contact me with any question about this package using this
[e-mail
address](mailto:leclerc.hadrien@gmail.com?subject=%5Br-package%5D%5Binsee%5D).

## Disclaimer

This package is in no way officially related to or endorsed by INSEE.
