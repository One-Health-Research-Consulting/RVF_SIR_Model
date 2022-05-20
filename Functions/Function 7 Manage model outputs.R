## create folder for outputs from hash of params vector


get_run_id <- function(param_vec){
  rlang::hash(list(param_vec,Sys.time()))
}

create_model_folders <- function(run_id,overwrite = FALSE){
  folder_path <- sprintf("%s/%s", "model_runs", run_id)
  dir.create(folder_path,recursive = TRUE)

  ## Create set of subfolders for different outputs ----  
  sub_folders <- c("Publication_Figures","Data for sensistivity analysis" )
  
  for(folder in sub_folders){
    sub_folder_path <- sprintf("%s/%s", folder_path, folder)
    dir.create(sub_folder_path,recursive = TRUE)
  }
  
  return(folder_path)
}


write_model_run_path <- function(folder_path){
  saveRDS(object = folder_path,"model_runs/current_run_path.RDS")
}

read_model_run_path <- function(){
  if(!file.exists("model_runs/current_run_path.RDS")){
    stop("File not found: Use create_model_run() to generate a new or 
         use set_model_run_path() to use previous a model model")
  }
  readRDS("model_runs/current_run_path.RDS")
}

delete_model_run_path <- function(){
  unlink("model_runs/current_run_path.RDS")
}

set_model_run_path <- function(run_id){
  folder_path <- sprintf("%s/%s", "model_runs", run_id)
  write_model_run_path(folder_path)
}

write_param_vec_rds <- function(folder_path, param_vec){
  param_vec_path <- sprintf("%s/%s",folder_path,"param_vec.RDS")
  saveRDS(object = param_vec,file = param_vec_path)
}
