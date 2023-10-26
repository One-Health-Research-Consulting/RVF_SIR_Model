#' ---
#' title: "Exploration of parameter space using Latin Hypercube"
#' author: "Louise Matthews and Mindy Rostal"
#' date: "July 9, 2022"
#' ---

#'Objective: Search the parameter space to estimate a value for the parameters that had no good estimates from the literature or expert opinion.  
#'
#'Parameter of interest:
#'* Aedes and Culex bite rates
#'* Aedes and Culex carrying capacity
#'* transovarial transmission fraction
#'* Aedes egg mortality rate
#' ###############################################
#Load libraries
library(codetools)
library(ggplot2)
library(deSolve)
library(zoo)
library(dplyr)
library(lhs)
library(parallel)
library(R.utils)
library(here)
library(tictoc)
library(gghighlight)

#Dectect cores:
core <- detectCores()

tic()
#Set scenario
SA <- TRUE
h <- 4000 #Number of simulations to run 

for (i in 1:10){ #Run 10 loops giving 40,000 runs
  initialise_RNG <- paste0(i, "234")
  set.seed(initialise_RNG)
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

# Load precipitation and Temp data 
  All_Precip <- read.csv(here("", "Combined Temp and Precip Data for RVF Simulation.csv"))


#Set timing
start.time <- 0
end.time <- nrow(All_Precip) 
timestep <- 1
times <- seq(from = start.time, 
             to = end.time, 
             by = timestep)


#We want to save the files in a folder named by the date
Datestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

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
                                                        Ae.dev.fun = dev_ALP, Cu.dev.fun = dev_CLP, 
                                                        ode.fun = ODE.RVFV.deterministic.SIRS, 
                                                        get.fun0 = Define.MosqYr, 
                                                        get.fun1 = Get.Extinct.Day, 
                                                        get.fun2 = Get.Max.Outbreak.Size.MosqYear, 
                                                        get.fun3 = Get.Mean.SeroP.MosqY, 
                                                        get.fun4 = Get.Mean.Outbreak.Size.MosqYear, 
                                                        get.fun5 = Get.Mean.Outbreak.lengths.MosqYear, 
                                                        get.fun6 = Get.Prop.Infected.Eggs, 
                                                        get.fun7 = Get.test1,
                                                        get.fun8 = Get.test2,
                                                        get.fun9 = Get.test3,
                                                        get.fun10 = Get.Mean.SeroP.MosqY1,
                                                        get.fun11 = Get.Mean.SeroP.MosqY2,
                                                        get.fun12 = Get.Mean.SeroP.MosqY3,
                                                        get.fun13 = Get.End.SeroP,
                                                        get.fun14 = Get.Mosq.Trends,
                                                        dev_param_vec = dev.params.set, 
                                                        pop_vec = initial.populations, 
                                                        timestp = times, file_date = Datestamp),
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
  "Get.Prop.Infected.Eggs_last_day",
  "Get.test1",
  "Get.test2",
  "Get.test3",
  "Get.Mean.SeroP.MosqY1",
  "Get.Mean.SeroP.MosqY2",
  "Get.Mean.SeroP.MosqY3",
  "Get.End.SeroP",
  "Get.Maxmax.IAE",
  "Get.Minmax.IAE",
  "Get.Maxmax.IC",
  "Get.Minmax.IC",
  "Get.Maxmax.IA",
  "Get.Minmax.IA",
  "Get.Slope.IAE"
  )

data$RNG_seed <- initialise_RNG
#Create file name
data_name  <- paste0("data_", "h", h, "_i", i, "_",Datestamp)
file_name <- paste0(data_name, ".Rdata")
dat.file <- paste("Data for sensitivity analyses/", file_name, sep = "")


# Save the data
assign(data_name, data)
save(data, file = dat.file)#Save the data


toc()
}



