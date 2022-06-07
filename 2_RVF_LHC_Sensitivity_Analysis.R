#' ---
#' title: "Latin Hypercube Sensitivity Analysis with lapply"
#' author: "Mindy Rostal"
#' date: "April 9, 2022"
#' ---

#'Objective: Conduct a sensitivity analysis on the RVF sheep model using a Latin hypercube. 
#'
#'Outcomes of interest:
#'* Does the virus persist
#'* Mean annual proportion of recovered animals (seroprevalence)
#'* Proportion of infected Aedes at the end of the study
#'* Mean length of an outbreak
#'* Mean size of an outbreak 
#'* Maximum size of a single outbreak
#' ###############################################
# #Load libraries
# library(codetools)
# library(ggplot2)
# library(deSolve)
# library(zoo)
# library(dplyr)
# library(lhs)
# library(parallel)
# library(R.utils)
# library(here)
RVF_LHC_SA <- function(target, h = 4000){
#Set scenario
SA <- TRUE
h <- h #Number of simulations to run in sensitivity anlaysis

#Source data and code
#Functions for setting the hatching switches the data
source(here("Functions", "Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R"))
#ODE equation
source(here("Model_Scripts", "Model 3 RVFV ODE SIRS function.R"))
#Load parameters
source(here("Model_Scripts", "Model 1 - RVFV Optimized Parameters.R"))
#Functions for pulling output of analysis
source(here("Functions", "Function 1 Define Functions for Output of Sensitivity Analysis.R"))
source(here("Functions", "Function 6 Latin Hypercube Analysis.R"))
source(here("Functions", "Function 7 Manage model outputs.R"))

model_run_path <- target

# Load precipitation and Temp data 
  All_Precip <- read.csv(here("", "Combined Temp and Precip Data for RVF Simulation.csv"))
  
#Set timing
start.time <- 0
end.time <- nrow(All_Precip) 
timestep <- 1
times <- seq(from = start.time, 
             to = end.time, 
             by = timestep)

#Dectect cores:
core <- detectCores()

#On my system, going above 20 cores would be counter productive
if(core>20 & h >20){
  core <- 20
}else{
  if(core > h){
    core <- h
  }
}

#We want to save the files in a folder named by the date
Datestamp <- format(Sys.Date(), "%Y_%m_%d")

#Turn matrix of hypercube parameters into a data.frame
params.matrix.list <- as.data.frame(params.matrix_trans)
params.matrix.list$Run <- seq(1, h, by = 1)
#Then turn into a list split by row
listargs <- split(params.matrix.list, 1:nrow(params.matrix.list))

data <- data.frame()
All_Precip1 <- data.frame()

#Run lapply
data.list <- mclapply(listargs, function(x) HypercubeSA(param_matrix = x,
                                                        empty.dat = data[0,],
                                                        empty.dat.precip = All_Precip1,
                                                        precip.dat = All_Precip,
                                                        precipdef.fun = DayDefinition,
                                                        Ae.endem.fun = AedesForcingEndemicMeanT,
                                                        Cu.fun = CulexForcingMeanT,
                                                        Ae.dev.fun = dev_ALP,
                                                        Cu.dev.fun = dev_CLP,
                                                        ode.fun = ODE.RVFV.deterministic.SIRS,
                                                        get.fun0 = Define.MosqYr,
                                                        get.fun1 = Get.Extinct.Day,
                                                        get.fun2 = Get.Max.Outbreak.Size.MosqYear,
                                                        get.fun3 = Get.Mean.SeroP.MosqY,
                                                        get.fun4 = Get.Mean.Outbreak.Size.MosqYear,
                                                        get.fun5 = Get.Mean.Outbreak.lengths.MosqYear,
                                                        get.fun6 = Get.Prop.Infected.Eggs,
                                                        dev_param_vec = dev.params.set,
                                                        pop_vec = initial.populations,
                                                        timestp = times,
                                                        file_date = Datestamp),
                      mc.cores = core, mc.preschedule = FALSE) 

#Turn results into a list
data.list <- as.list(data.list)

#Turn into data.frame
data <- do.call("rbind", data.list)

#Rename columns
names(data) <- c(
  colnames(params.matrix_trans),
  "Run", 
  "Extinction.Day", 
  "Max.Outbreak.Size.MosqYear", 
  "Mean.Annual.Seroprevalence", 
  "Get.Mean.Outbreak.Size.MosqYear", 
  "Get.Mean.Outbreak.lengths.MosqYear",
  "Get.Prop.Infected.Eggs_last_day"
  )

#Create file name

dat.file <- paste(model_run_path, "/Data for sensitivity analyses/SA_trans_Publication.Rdata", sep = "")


# 
# Save the data
save(data, file = dat.file)#Save the data

}
