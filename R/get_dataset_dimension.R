#' @noRd
get_dataset_dimension = function(dataset){

  insee_sdmx_link_datastructure = Sys.getenv("INSEE_sdmx_link_datastructure")
  link = file.path(insee_sdmx_link_datastructure, dataset)

  dir_creation_fail = try(create_insee_folder(), silent = TRUE)

  if(!"try-error" %in% class(dir_creation_fail)){
    insee_local_dir = rappdirs::user_data_dir("insee")
  }else{
    insee_local_dir = tempdir()
  }

  dataset_dim_file_cache = file.path(insee_local_dir,
                                     paste0(openssl::md5(link), ".rds"))

  if(!file.exists(dataset_dim_file_cache)){

    response = try(httr::GET(link), silent = TRUE)
    response_content = try(httr::content(response, encoding = "UTF-8"), silent = TRUE)

    content_list = xml2::as_list(response_content)
    data = tibble::as_tibble(content_list)

    l = try(data[[1]][[2]]$DataStructures$DataStructure$DataStructureComponents$AttributeList$Attribute$AttributeRelationship, silent = TRUE)

    if(class(l) != "try-error"){
      if(!is.null(l)){
        list_dimension = unlist(lapply(1:length(l), function(i){return(attr(l[[i]]$Ref,"id"))}))

        saveRDS(list_dimension, file = dataset_dim_file_cache)

      }else{return(NULL)}
    }else{return(NULL)}

  }else{
    list_dimension = readRDS(file = dataset_dim_file_cache)
  }

  return(list_dimension)
}
