#' Get the title of dataset's columns
#'
#' @param dataset an INSEE's dataset, if NULL
#' @return a dataframe
#' @examples
#' \donttest{
#' column_titles_all_dataset = get_column_title()
#'
#' column_titles = get_column_title("CNA-2014-CONSO-MEN")
#' }
#' @export
get_column_title = function(dataset = NULL){

  if(!is.null(dataset)){
    dataset_dimension = get_dataset_dimension(dataset = dataset)
  }else{
    list_dataset = as.character(get_dataset_list()$id)
    dataset_dimension = unique(unlist(
      lapply(1:length(list_dataset),
                      function(i){
                        dataset_dimension = get_dataset_dimension(dataset = list_dataset[i])
                        return(dataset_dimension)
                      }
                        )))
  }

  if(!is.null(dataset_dimension)){
    dataset_dimension = sort(dataset_dimension)

    dimension_name_df = dplyr::bind_rows(
      lapply(1:length(dataset_dimension),
             function(i){
               df_dim = get_dimension_values(dimension = dataset_dimension[i], name = TRUE)
               return(df_dim)
               })
    )

    dimension_name_df[,"dimension"] = gsub("-", "_", dimension_name_df[,"dimension"])

    dimension_name_df = tibble::as_tibble(dimension_name_df)

    return(dimension_name_df)
  }else{
    return(NULL)
  }
}
