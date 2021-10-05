
library(insee)
library(tidyverse)

# idbank_list = build_idbank_list( c('IPC-2015', 'BALANCE-PAIEMENTS'))
# idbank_list = build_idbank_list()


build_idbank_list = function(dataset_list = NULL){

  if(is.null(dataset_list)){
    dataset_list = get_dataset_list() %>%
      filter(id != 'SERIES_BDM') %>%
      pull(id)
  }

  mutate_paste <- function(data, vars){
    .vars <- rlang::syms(vars)

    result <- data %>%
      group_by(IDBANK) %>%
      mutate(new = paste0(c(!!!.vars), collapse = "."))

    return(result)
  }

  pb = utils::txtProgressBar(min = 1, max = length(dataset_list), initial = 1, style = 3)


  idbank_list = dplyr::bind_rows(
    lapply(1:length(dataset_list),
           function(i){
             dataset = dataset_list[i]

             dataset_dimension = insee:::get_dataset_dimension(dataset)

             df = get_insee_dataset(dataset, firstNObservations = 1)

             if(is.null(df)){
               link = sprintf("https://bdm.insee.fr/series/sdmx/data/%s", dataset)

               response = try(httr::GET(link), silent = TRUE)

               response_content = try(httr::content(response, encoding = "UTF-8"), silent = TRUE)
               content_list = xml2::as_list(response_content)

               content_query = content_list$Error$ErrorMessage
               lgt = length(content_query)

               list_query = paste0(unlist(lapply(1:lgt,
                                                 function(i){content_query[[i]][1]})),'?firstNObservations=1')

               df = dplyr::bind_rows(
                 lapply(1:lgt,
                        function(i){
                          get_insee(list_query[i])
                        })
               )

             }

             if(all(c('IDBANK', dataset_dimension) %in% names(df))){
               df_id = df %>%
                 select(IDBANK, !!dataset_dimension, TITLE_FR, TITLE_EN) %>% #
                 mutate_paste(vars = dataset_dimension) %>%
                 rename(cleflow = new) %>%
                 mutate(nomflow = dataset) %>%
                 select(nomflow, IDBANK, cleflow)

               utils::setTxtProgressBar(pb, i)
               return(df_id)
             }

           })
  )

  return(idbank_list)
}


