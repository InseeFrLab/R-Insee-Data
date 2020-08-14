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
[![Build Status](https://travis-ci.com/hadrilec/insee.svg?branch=master)](https://travis-ci.org/hadrilec/insee)

<br>

## Overview

The insee package contains tools to easily download data and metadata
from INSEE main database (BDM).

Using embedded SDMX queries, get the data of more than 140 000 INSEE
series.

Have a look at the detailed SDMX web service page with the following
[link](https://www.insee.fr/en/information/2868055).

This package is a contribution to reproducible research and public data
transparency.

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

  - [Tutorial](https://hadrilec.github.io/insee/articles/insee.html)
  - [GDP growth
    rate](https://hadrilec.github.io/insee/articles/v2_gdp-vignettes.html)
  - [Inflation](https://hadrilec.github.io/insee/articles/v3_inflation-vignettes.html)
  - [Unemployment
    rate](https://hadrilec.github.io/insee/articles/v4_unem-vignettes.html)
  - [Population by
    age](https://hadrilec.github.io/insee/articles/v5_pop-vignettes.html)
  - [Population
    map](https://hadrilec.github.io/insee/articles/v6_pop_map-vignettes.html)

## How to avoid proxy issues ?

``` r
Sys.setenv(http_proxy = "my_proxy_server")
Sys.setenv(https_proxy = "my_proxy_server")
```

## Other useful packages

  - [rsdmx](https://cran.r-project.org/web/packages/rsdmx/index.html)
    and
    [eurostat](https://cran.r-project.org/web/packages/eurostat/index.html)
    packages were my starting points when I first started to play with
    INSEE’s data. They are of great help to retrieve data from all over
    Europe. However, the
    [insee](https://cran.r-project.org/web/packages/insee/index.html)
    package might be better to access INSEE’s data, for the following
    reasons.
  - **Full metadata coverage** : with the `get_idbank_list` function you
    have access to all INSEE’s series keys, which is crucial to find the
    right data. Moreover, with the `add_insee_title` function it is easy
    to get titles from the metadata table.
  - **Enhanced data formatting**: a column in Date format is added to
    the raw data and the OBS\_VALUE column is in the numeric format,
    which good to make plots an computation.
  - **Better queries** : with rsdmx it can be hard to know how to design
    a query whereas with `get_insee_idbank` and `get_insee_dataset` it
    is straighforward. Then, with `get_insee_idbank` the user can
    trigger automatically multiple queries bypassing the sdmx limit of
    400 series. Finaly, all the data retrieved is cached, so all queries
    are run only once per R session which is not the case in the
    [rsdmx](https://cran.r-project.org/web/packages/rsdmx/index.html)
    package.
  - **Proxy issues solved**

<center>

| Task                                   | [insee](https://cran.r-project.org/web/packages/insee/index.html) | [rsdmx](https://cran.r-project.org/web/packages/rsdmx/index.html) | [eurostat](https://cran.r-project.org/web/packages/eurostat/index.html) |
| -------------------------------------- | ----------------------------------------------------------------- | ----------------------------------------------------------------- | ----------------------------------------------------------------------- |
| Full INSEE’s Metadata Coverage         | ✅                                                                 | :x:                                                               | :x:                                                                     |
| Enhanced data formatting               | ✅                                                                 | :x:                                                               | ✅                                                                       |
| Cached data                            | ✅                                                                 | :x:                                                               | ✅                                                                       |
| Query in bulk with INSEE’s series keys | ✅                                                                 | :x:                                                               | :x:                                                                     |
| Retrieve data from all over Europe     | :x:                                                               | ✅                                                                 | ✅                                                                       |
| Avoid proxy issues easily              | ✅                                                                 | :x:                                                               | ✅                                                                       |

</center>

## Support

Feel free to contact me with any question about this package using this
[e-mail
address](mailto:leclerc.hadrien@gmail.com?subject=%5Br-package%5D%5Binsee%5D).

## Disclaimer

This package is in no way officially related to or endorsed by INSEE.
