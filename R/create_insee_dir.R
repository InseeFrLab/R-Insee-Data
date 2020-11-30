#' @noRd
create_insee_dir = function(){

  insee_folder = file.path(rappdirs::user_data_dir(), "insee")
  list_folders = c(insee_folder, file.path(insee_folder, "insee"))

  for(ifile in 1:length(list_folders)){
    if(!file.exists(list_folders[ifile])){
      dir_creation = dir.create(list_folders[ifile])
    }
  }

}
