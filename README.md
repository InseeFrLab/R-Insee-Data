
## Overview

The insee package contains tools to easily download data and metadata from INSEE BDM database.
Using embedded SDMX queries, get the data of more than 140 000 INSEE series from BDM database (Banque de données macroéconomiques).

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

# get INSEE datasets list
```{r dataset list}
dataset = get_dataset_list()
```

# get INSEE series key (idbank) list
```{r idbank list}
idbank_list = get_idbank_list()
```

# select idbanks 
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

# extract selected idbanks list
```{r selected idbank}
list_idbank = idbank_list_selected %>% pull(idbank)
```

# get selected idbanks data
```{r data}
data = get_insee_idbank(list_idbank)
```

# avoid proxy issues 
```{r proxy}
Sys.setenv(http_proxy = "my_proxy_server")
Sys.setenv(https_proxy = "my_proxy_server")
```

# Full example : French GDP plot
```{r GDP}

idbank_list = get_idbank_list()

idbank_list_selected =
  idbank_list %>%
  filter(nomflow == "CNT-2014-PIB-EQB-RF") %>%  # Gross domestic product balance
  filter(dim1 == "T") %>% #quarter
  filter(dim4 == "PIB") %>% #GDP
  filter(dim6 == "TAUX") #rate

idbank = idbank_list_selected %>% pull(idbank)

idbank_title = get_insee_title(idbank)

data = get_insee_idbank(idbank) %>% 
  mutate(OBS_VALUE = as.numeric(as.character(OBS_VALUE)))

ggplot(data, aes(x = TIME, y = OBS_VALUE)) +
  geom_line() +
  ggtitle("French GDP growth rate")
```
