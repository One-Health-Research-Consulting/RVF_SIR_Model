## create folder for outputs from hash of params vector

get_folder_id <- function(params){
  rlang::hash(list(params,Sys.time()))
}

create_model_folders <- function(folder_id,overwrite = FALSE){
  folder_path <- sprintf("%s/%s", "model_runs", folder_id)
  dir.create(folder_path,recursive = TRUE)

  ## Create set of subfolders for different outputs ----  
  sub_folders <- c("Publication_Figures","Data for sensistivity analysis", )
  
  for(folder in sub_folders){
    sub_folder_path <- sprintf("%s/%s", folder_path, folder)
    dir.create(sub_folder_path,recursive = TRUE)
  }
  
  return(folder_path)
}

