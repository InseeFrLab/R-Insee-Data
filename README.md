
## Overview

The insee package contains tools to download easily data and metadata from INSEE BDM database.

## Installation

```{r eval = FALSE}
# Get the development version from GitHub
# install.packages("devtools")
devtools::install_github("hadrilec2/insee")
```

## Usage

```{r example}
library(tidyverse)
```

You can see conflicts created later with `tidyverse_conflicts()`:

```{r conflicts}
library(MASS)
tidyverse_conflicts()
```

And you can check that all tidyverse packages are up-to-date with `tidyverse_update()`:

```{r update, eval = FALSE}
tidyverse_update()
#> The following packages are out of date:
#>  * broom (0.4.0 -> 0.4.1)
#>  * DBI   (0.4.1 -> 0.5)
#>  * Rcpp  (0.12.6 -> 0.12.7)
#> Update now?
#> 
#> 1: Yes
#> 2: No
```


