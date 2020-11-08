#' Remove columns filled only with NA (missing value)
#'
#' @param df the dataframe to be cleaned
#' @return a dataframe without columns containing only NA
#' @examples
#' \donttest{
#'library(tidyverse)
#'
#'idbank_empl =
#'  get_idbank_list("EMPLOI-SALARIE-TRIM-NATIONAL") %>% #employment
#'  mutate(title = get_insee_title(idbank)) %>%
#'  separate(title, sep = " - ", into = paste0("title", 1:5), fill = "right") %>%
#'  clean_table()
#' }
#' @export
clean_table = function(df){
  df[, colSums(is.na(df)) != nrow(df)]
}
