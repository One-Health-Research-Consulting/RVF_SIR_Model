# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.
source("packages.R")

# Load the R scripts with your custom functions:
for (file in list.files("Functions", full.names = TRUE)) source(file)
for(run_funcs in list.files(pattern = "\\d[-_]",full.names = TRUE)) source(run_funcs)

# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    name = Create_Run_Path,
    command = create_model_run_path(),
    cue = targets::tar_cue("always")
  ),
  tar_target(
    name = RVFV_SIM,
    command = run_rvfv_simulations(Create_Run_Path),
    cue = targets::tar_cue("always")
  ),
  # tar_target(
  #   name = LHC_SA,
  #   command = source("2_RVF_LHC_Sensitivity_Analysis.R"),
  #   cue = targets::tar_cue("never") ## takes around 100hrs on 8 core machine
  # ),
  tar_target(
    name = Calc_Plot_R0,
    command = calculate_plot_R0(RVFV_SIM),
    cue = targets::tar_cue("always")
  ),
  tar_target(
    name = Remove_Run_Path,
    command = delete_model_run_path(Calc_Plot_R0),
    cue = targets::tar_cue("always")
  )
)
