#' @noRd
create_insee_folder = function(){

  insee_folder = file.path(rappdirs::user_data_dir(), "insee")
  list_folders = c(insee_folder, file.path(insee_folder, "insee")
                   # ,file.path(insee_folder, "insee", "data")
                   )

  for(ifile in 1:length(list_folders)){
    if(!file.exists(list_folders[ifile])){
      dir_creation = dir.create(list_folders[ifile])
    }
  }

  test_file = file.path(insee_folder, "insee", paste0(openssl::md5("test_file"), ".rds"))
  if(!file.exists(test_file)){
    test = 1
    saveRDS(test, file = test_file)
  }

  # # files deleted after 2 hours
  # list_file_insee_data_dir = file.path(file.path(insee_folder, "insee", "data"),
  #                                      list.files(path = file.path(insee_folder, "insee", "data")))
  #
  # date_file_modif = file.info(list_file_insee_data_dir)$mtime
  # diff_time_files = difftime(Sys.time(), date_file_modif, units = "hours")
  #
  # old_files = list_file_insee_data_dir[diff_time_files > 2]
  #
  # # file deletion
  # if(length(old_files) > 0){
  #   invisible(file.remove(old_files))
  # }

}
