#' @noRd
read_dataset_metadata = function(dataset, dataset_metadata_file_cache){

  dir_creation_fail = try(create_insee_folder(), silent = TRUE)

  if(!"try-error" %in% class(dir_creation_fail)){
    insee_local_dir = file.path(rappdirs::user_data_dir(), "R", "insee", "insee")
  }else{
    insee_local_dir = tempdir()
  }

  if(missing(dataset_metadata_file_cache)){

    dataset_hash = paste0(dataset, collapse = "_")

    dataset_metadata_file_cache = file.path(insee_local_dir,
                                            paste0(openssl::md5(sprintf("%s_metadata_file", dataset_hash)), ".rds"))
  }

  if(!file.exists(dataset_metadata_file_cache)){

    if(!is.null(dataset)){
      sub_dataset_metadata_file = unlist(lapply(dataset,
                                                function(dt){
                                                  return(file.path(insee_local_dir,
                                                                   paste0(openssl::md5(sprintf("%s_metadata_file", dt)), ".rds")))
                                                }))

      if(all(file.exists(sub_dataset_metadata_file))){

        idbank_list = dplyr::bind_rows(lapply(sub_dataset_metadata_file,
                                              function(dataset_file){return(readRDS(dataset_file))}))

        idbank_list = set_metadata_col(idbank_list)

        saveRDS(idbank_list, file = dataset_metadata_file_cache)

        return(idbank_list)

      }else{

        missing_dataset_metadata = paste0(dataset[!file.exists(sub_dataset_metadata_file)], collapse = " ")
        msg = "\nMetadata is missing for the following datasets : "
        message(crayon::style(sprintf("%s %s", msg, missing_dataset_metadata), "red"))

        return(TRUE)
      }
    }
  }else{

    idbank_list = readRDS(dataset_metadata_file_cache)
    message(crayon::style("Cached data has been used", "green"))

    return(idbank_list)

  }

}
