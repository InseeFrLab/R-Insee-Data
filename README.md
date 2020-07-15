---
output: github_document
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
```

## Overview

The insee package contains tools to download easily data and metadata from INSEE BDM database.

## Installation
```{r eval = FALSE}
# Get the development version from GitHub
# install.packages("devtools")
devtools::install_github("hadrilec2/insee")
```

# Library
```{r example, echo = FALSE}
library(tidyverse)
library(insee)
```

# INSEE dataset list
```{r dataset list}
dataset = get_dataset_list()
```

# INSEE series key (idbank) list
```{r idbank list}
idbank_list = get_idbank_list()
```

# select idbank 
```{r select idbank}
idbank_list_selected = 
  idbank_list %>% 
  filter(nomflow == "ENQ-CONJ-ACT-IND") %>% 
  filter(dim12 == "A88-29") %>% 
  filter(dim8  == "CVS") %>% 
  filter(dim13 == "SOLDE_PROPORTION") %>% 
  filter(dim10 == "ECAI_TPE") 
```
  
# get idbank title
```{r get_title}
idbank_list_selected = 
  idbank_list_selected %>% 
  mutate(title = get_insee_title(idbank, lang = "fr")) 
```

# extract selected idbank list
```{r selected idbank}
list_idbank = idbank_list_selected %>% pull(idbank)
```

# get selected idbanks data
```{r data}
data = get_insee_idbank(list_idbank)
```


