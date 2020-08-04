## ----setup,echo=FALSE, include=FALSE------------------------------------------
# setup chunk
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")),"true")
knitr::opts_chunk$set(purl = NOT_CRAN)
library(insee)
library(tidyverse)

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(kableExtra)
library(magrittr)
library(htmltools)
library(prettydoc)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  Sys.setenv(http_proxy = "my_proxy_server")
#  Sys.setenv(https_proxy = "my_proxy_server")

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  install.packages("insee")

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  library(insee)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  library(tidyverse)

## ---- message = FALSE, warning = FALSE, eval = NOT_CRAN-----------------------
insee_dataset <- get_dataset_list() 

## ----echo = FALSE, message = FALSE, warning = FALSE, eval = NOT_CRAN----------
rownames(insee_dataset) <- NULL

insee_dataset %>% 
  select(id, Name.en, Name.fr, url, n_series) %>% 
  slice(1:10) %>% 
  kable(row.names=NA) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=NOT_CRAN-------------------------------
idbank_list = get_idbank_list()

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=NOT_CRAN-------------------
rownames(idbank_list) <- NULL
idbank_list %>% 
          select(nomflow, idbank, cleFlow) %>%   
          group_by(nomflow) %>% 
          slice(1) %>% 
          ungroup() %>% 
          head(10) %>%
          kable(row.names=NA) %>% 
          kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  idbank_list = get_idbank_list()
#  
#  idbank_list_selected =
#    idbank_list %>%
#    filter(nomflow == "IPI-2015") %>% #industrial production index dataset
#    filter(dim1 == "M") %>% #monthly
#    filter(dim5 == "INDICE") %>% #index
#    filter(dim8 == "CVS-CJO") %>% #Working day and seasonally adjusted SA-WDA
#    #automotive industry and overall industrial production
#    filter(str_detect(dim4,"^29$|A10-BE")) %>%
#    mutate(title = get_insee_title(idbank))
#  
#  idbank_list_selected
#  

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  library(tidyverse)
#  
#  # the user can make a manual list of idbanks to get the data
#  # example 1
#  
#  data = get_insee_idbank("001558315", "010540726")
#  
#  # using a list of idbanks extracted from the insee idbank dataset
#  # example 2 : household's confidence survey
#  
#  idbank_dataset = get_idbank_list()
#  
#  df_idbank = idbank_dataset %>%
#    filter(nomflow == "ENQ-CONJ-MENAGES") %>%  #monthly households' confidence survey
#    mutate(title = get_insee_title(idbank)) %>%
#    separate(title, sep = " - ", into = paste0("title", 1:4),
#             fill = "right", remove = FALSE) %>%
#    filter(dim7 == "CVS") #seasonally adjusted
#  
#  # dim2: IND_SYNT_CONF - Summary indicator of households' confidence
#  
#  list_idbank = df_idbank %>% pull(idbank)
#  
#  data = get_insee_idbank(list_idbank)
#  

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  
#  insee_dataset <- get_dataset_list()
#  
#  # example 1 : full dataset
#  data = get_insee_dataset("CLIMAT-AFFAIRES")
#  
#  # example 2 : filtered dataset
#  # the user can filter the data
#  data = get_insee_dataset("IPC-2015", filter = "M+A.........CVS.", startPeriod = "2015-03")
#  
#  # in the filter, the + is used to select several values in one dimension, like an "and" statement
#  # the void means "all" values available
#  
#  # example 3 : only one series
#  # by filtering with the full SDMX series key, the user will get only one series
#  data =
#    get_insee_dataset("CNA-2014-CPEB",
#                      filter = "A.CNA_CPEB.A38-CB.VAL.D39.VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT",
#                      lastNObservations = 10)
#  

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  library(tidyverse)
#  
#  idbank_list = get_idbank_list()
#  
#  df_idbank_list_selected =
#    idbank_list %>%
#    filter(nomflow == "CNT-2014-PIB-EQB-RF") %>% # Gross domestic product balance
#    filter(dim1 == "T") %>% #quarter
#    filter(dim4 == "PIB") %>% #GDP
#    filter(dim6 == "TAUX") %>% #rate
#    filter(dim10 == "CVS-CJO") #SA-WDA, seasonally adjusted, working day adjusted
#  
#  idbank = df_idbank_list_selected %>% pull(idbank)
#  
#  data = get_insee_idbank(idbank)
#  
#  #plot
#  ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
#    geom_col() +
#    ggtitle("French GDP growth rate, quarter-on-quarter, sa-wda") +
#    labs(subtitle = sprintf("Last updated : %s", data$TIME_PERIOD[1]))
#  

## ----message = FALSE, warning=FALSE, eval = FALSE-----------------------------
#  library(tidyverse)
#  library(lubridate)
#  
#  idbank_list = get_idbank_list()
#  
#  df_idbank_list_selected =
#    idbank_list %>%
#    filter(nomflow == "IPC-2015") %>% #Inflation dataset
#    filter(dim1 == "M") %>% # monthly
#    filter(str_detect(dim4, "^[0-9]{2}$")) %>% # coicop aggregation level
#    filter(dim6 == "INDICE") %>% # index
#    filter(dim7 == "ENSEMBLE") %>% # all kinds of household
#    filter(dim8 == "FE") %>% # all France including overseas departements
#    mutate(title = get_insee_title(idbank))
#  
#  list_idbank = df_idbank_list_selected %>% pull(idbank)
#  
#  data = get_insee_idbank(list_idbank, startPeriod = "2010-01")
#  
#  n_sep = str_count(data$TITLE_FR[1], " - ") + 1
#  
#  data_plot = data %>%
#    separate(TITLE_EN, into = paste0("title", 1:n_sep),
#             sep = " - ", remove = FALSE, fill = "right") %>%
#    mutate(title6 = case_when(is.na(title6) ~ title5,
#                              TRUE ~ as.character(title6))) %>%
#    mutate(title6 = substr(title6, 1 , 22)) %>%
#    mutate(month = month(DATE)) %>%
#    arrange(DATE) %>%
#    group_by(title6, month) %>%
#    mutate(growth = 100 * (OBS_VALUE / dplyr::lag(OBS_VALUE) - 1))
#  
#  ggplot(data_plot, aes(x = DATE, y = growth)) +
#    geom_col() +
#    facet_wrap(~title6, scales = "free") +
#    ggtitle("French inflation, by product categories, year-on-year") +
#    labs(subtitle = sprintf("Last updated : %s", data_plot$TIME_PERIOD[nrow(data_plot)]))
#  

## ----message=FALSE, warning = FALSE, eval = FALSE-----------------------------
#  library(tidyverse)
#  
#  dataset_list = get_dataset_list()
#  
#  idbank_list = get_idbank_list()
#  
#  df_idbank_list_selected =
#    idbank_list %>%
#    filter(nomflow == "CHOMAGE-TRIM-NATIONAL") %>% #Unemployment dataset
#    mutate(title = get_insee_title(idbank)) %>%
#    filter(dim2 == "CTTXC") %>% #unemployment rate based on ILO standards
#    filter(dim4 == "FE") %>%  # all France including overseas departements
#    filter(dim5 == 0) # men and women
#  
#  list_idbank = df_idbank_list_selected %>% pull(idbank)
#  
#  data = get_insee_idbank(list_idbank, startPeriod = "2000-01")
#  
#  n_sep = str_count(data$TITLE_FR[1], " - ") + 1
#  
#  data_plot = data %>%
#    separate(TITLE_EN, into = paste0("title", 1:n_sep),
#             sep = " - ", remove = FALSE, fill = "right")
#  
#  ggplot(data_plot, aes(x = DATE, y = OBS_VALUE, colour = title2)) +
#    geom_line() +
#    geom_point() +
#    ggtitle("French unemployment rate, by age") +
#    labs(subtitle = sprintf("Last updated : %s", data_plot$TIME_PERIOD[1]))
#  

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  library(tidyverse)
#  
#  dataset_list = get_dataset_list()
#  
#  idbank_list = get_idbank_list()
#  
#  df_idbank_list_selected =
#    idbank_list %>%
#    filter(nomflow == "POPULATION-STRUCTURE") %>% #population dataset
#    mutate(title = get_insee_title(idbank)) %>%
#    filter(dim2 == "POPULATION_1ER_JANVIER") %>% #population at the beginning of the year
#    filter(dim5 == "FE") %>%  # all France including overseas departements
#    filter(dim6 == 0) %>%  # men and women
#    filter(dim7 %in% c("00-19", "20-59", "60-")) #age ranges
#  
#  list_idbank = df_idbank_list_selected %>% pull(idbank)
#  
#  data = get_insee_idbank(list_idbank)
#  
#  n_sep = str_count(data$TITLE_FR[1], " - ") + 1
#  
#  data_plot = data %>%
#    separate(TITLE_EN, into = paste0("title", 1:n_sep),
#             sep = " - ", remove = FALSE, fill = "right") %>%
#    mutate(OBS_VALUE = OBS_VALUE / 10^6)
#  
#  ggplot(data_plot, aes(x = DATE, y = OBS_VALUE, fill = title3)) +
#    geom_area() +
#    ggtitle("French population in millions, by age") +
#    labs(subtitle = sprintf("Last updated : %s", data_plot$TIME_PERIOD[1]))
#  

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  
#  library(insee)
#  library(tidyverse)
#  
#  library(raster)
#  library(rgdal)
#  library(broom)
#  library(viridis)
#  
#  idbank_list = get_idbank_list()
#  
#  dataset_list = get_dataset_list()
#  
#  list_idbank = idbank_list %>%
#    filter(nomflow == "TCRED-ESTIMATIONS-POPULATION") %>%
#    filter(dim6 == "00-") %>% #all population
#    filter(dim5 == 0) %>% #men and women
#    filter(str_detect(dim4, "^D")) %>% #departement
#    mutate(title = get_insee_title(idbank))
#  
#  list_idbank_selected = list_idbank %>% pull(idbank)
#  
#  # get population data by departement
#  pop = get_insee_idbank(list_idbank_selected)
#  
#  #get departement limits
#  FranceMap <- raster::getData(name = "GADM", country = "FRA", level = 2)
#  
#  # extract the population by departement in 2020
#  pop_plot = pop %>%
#    group_by(TITLE_EN) %>%
#    filter(DATE == "2020-01-01") %>%
#    mutate(dptm = gsub("D", "", REF_AREA)) %>%
#    filter(dptm %in% FranceMap@data$CC_2) %>%
#    mutate(dptm = factor(dptm, levels = FranceMap@data$CC_2)) %>%
#    arrange(dptm) %>%
#    mutate(id = dptm)
#  
#  vec_pop = pop_plot %>% pull(OBS_VALUE)
#  
#  # add population data to the departement object map
#  FranceMap@data$pop = vec_pop
#  
#  # extract the departement limits from the spatial object
#  FranceMap_tidy <- broom::tidy(FranceMap)
#  
#  # mapping table
#  dptm_df = data.frame(dptm = FranceMap@data$CC_2,
#                       dptm_name = FranceMap@data$NAME_2,
#                       pop = FranceMap@data$pop,
#                       id = rownames(FranceMap@data))
#  
#  # add population data to departement dataframe
#  FranceMap_tidy_final =
#    FranceMap_tidy %>%
#    left_join(dptm_df, by = "id") %>%
#    select(long, lat, pop, group, id)
#  
#  ggplot() +
#    geom_polygon(data = FranceMap_tidy_final,
#                 aes(fill = pop, x = long, y = lat, group = group) ,
#                 size = 0, alpha = 0.9) +
#    coord_map() +
#    theme_void() +
#    scale_fill_viridis() +
#    ggtitle("Distribution of the population on French territory in 2020")
#  

