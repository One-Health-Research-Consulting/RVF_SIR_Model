###########
#' Author: Mindy Rostal
#' Title: Function for sensitivity analysis
#' date: April 10, 2022
#' ##################
#' 
#' HypercubeSA
#' Purpose: Create a function to conduct the sensitivity analysis and that can be used in an mclapply command.
#' The main simulation code from 1-Run RVFV Simulations.R and Model 2 Mosquito hatch 
#' rates at daily timestep.R included in the function. After the simulation is run, 
#' six outcome functions are run (get.fun1-6). They are detailed when the function 
#' is called, but must be left in order as some require specific arguments and these 
#' are hard coded into the function. Further, they need to be in order so that the 
#' column names applied after the function is run labels the results correctly. If 
#' needed an option to save each simulation data file can be uncommented and used to 
#' evaluate the actual simulation if something more than the output summary is required. 
#' This was designed to run within an mclapply function.
#' 
#' @param param_matrix The dataframe of parameters and lhs variables from "Model 1 - RVFV Selected Parameters.R and split by rows
#' @param empty.dat Empty dataframe for results
#' @param empty.dat.precip Empty dataframe for calculating climate-based variables
#' @param precip.dat The climate dataframe
#' @param precipdef.fun the DayDefinition function from Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R
#' @param Ae.endem.fun the AedesForcingEndemicMeanT function from from Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R
#' @param Cu.fun the CulexForcingMeanT function from Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R
#' @param Ae.dev.fun the dev_ALP function from function from Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R
#' @param Cu.dev.fun the dev_CLP from function from Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R
#' @param ode.fun the ODE.RVFV.deterministic.SIRS from Model 3 RVFV ODE SIRS function.R
#' @param get.fun0 the Define.MosqYr function from Function 1 Define Functions for Output of Sensitivity Analysis.R
#' @param get.fun1 the Get.Extinct.Day function from Function 1 Define Functions for Output of Sensitivity Analysis.R
#' @param get.fun2 the Get.Max.Outbreak.Size.MosqYear function from Function 1 Define Functions for Output of Sensitivity Analysis.R
#' @param get.fun3 the Get.Mean.SeroP.MosqY function from Function 1 Define Functions for Output of Sensitivity Analysis.R
#' @param get.fun4 the Get.Mean.Outbreak.Size.MosqYear function from Function 1 Define Functions for Output of Sensitivity Analysis.R
#' @param get.fun5 the Get.Mean.Outbreak.lengths.MosqYear function from Function 1 Define Functions for Output of Sensitivity Analysis.R
#' @param get.fun6 the Get.Prop.Infected.Eggs function from Function 1 Define Functions for Output of Sensitivity Analysis.R
#' @param dev_param_vec the dev.params.set from Rueda et al listed in Model 1 - RVFV Selected Parameters.R
#' @param pop_vec the initial populations vector from Model 1 - RVFV Selected Parameters.R
#' @param timestp the sequence of timesteps from the start to end of the model (1 day = 1 time step)
#' @param file_date the Sys.Date() to save files in folder by date

HypercubeSA <- function(param_matrix, empty.dat, empty.dat.precip, precip.dat, precipdef.fun, Ae.endem.fun, Cu.fun, Ae.dev.fun, Cu.dev.fun, ode.fun, get.fun0, get.fun1, get.fun2, get.fun3, get.fun4, get.fun5, get.fun6, dev_param_vec, pop_vec, timestp, file_date){ 
  print(1.05)
  flush.console()
  
  print(1.1)
  param <- as.list(c(param_matrix))
  #Set out to be an empty dataframe
  print(1.15)
  out <- data.frame()
  #Set hatching data up
  print(1.2)

  #First run the DayDefinition function 
  #Add columns the function will fill in
  precip.dat$hatchingA <- FALSE
  precip.dat$Days_rainyA <- 0# Has to get to 0 before hatching is permitted
  precip.dat$Days_hatchingA <- 0 # Hatching happens until this gets to CountDayRainA number of days
  precip.dat$hatchingC <- FALSE
  print(1.26)
  empty.dat.precip <- precipdef.fun(precip.dat, 
                                    rollcumrainA15 = param[["DaysCumRainA"]], 
                                    #rainspreadperiodepi = param[["FrequencyofRainDaysOver"]], 
                                    cumrainC15 = param[["DaysCumRainC"]], 
                                    minrainC = param[["MinRainC"]], 
                                    minrainA = param[["MinRainA"]], 
                                    Htemp = param[["TempAC"]])
  print(1.27)
  # Now define the aedes hatching days - MUST do _Aedes_ endemic days first!
  empty.dat.precip <- Ae.endem.fun(empty.dat.precip, hatchabledayA = param[["DaysCanHatchA"]])
  print(1.28)
  # Now define the culex hatching days
  empty.dat.precip <- Cu.fun(empty.dat.precip, dayspostAhatch = param[["CulexHatchDelay"]])
  print(1.3)
  #Calculated development rates
  empty.dat.precip <- as.data.frame(empty.dat.precip)
  #Aedes
  empty.dat.precip <- Ae.dev.fun(dat = empty.dat.precip,
                                 rh025 = dev_param_vec["rh025a"], 
                                 HA=dev_param_vec["HAa"], 
                                 TH=dev_param_vec["THa"], 
                                 HH=dev_param_vec["HHa"])
  
  #Culex 
  empty.dat.precip <- Cu.dev.fun(dat = empty.dat.precip, 
                                 rh025 = dev_param_vec["rh025c"], 
                                 HA= dev_param_vec["HAc"], 
                                 TH= dev_param_vec["THc"], 
                                 HH= dev_param_vec["HHc"])
  
  #Add additional column
  empty.dat.precip <- empty.dat.precip%>%
    mutate(AedesEpiHatchMeanT = FALSE)#
  
  
  #Add vaccination pattern - none, daily, burst
  empty.dat.precip <- empty.dat.precip%>%
    mutate(No_vax = 0,
           Daily_vax = 1,
           Burst_vax = 0)
  
  #ODE model
  #Set up aproxfun data
  #Hatching for Aedes
  print(2.1)
  sigimpAMean <- approxfun(empty.dat.precip$SimDay, empty.dat.precip$hatchingATMean, rule = 2, method = "linear")
  print(2.2)
  # Hatching for Culex
  sigimpCMean <- approxfun(empty.dat.precip$SimDay, empty.dat.precip$hatchingCPropTMean, rule = 2, method = "linear")
  print(2.3)
  #Temp dependent development for Aedes and Culex
  sigimp_dev_ALP <- approxfun(empty.dat.precip$SimDay, empty.dat.precip$Dev_ALP, rule = 2, method = "linear")
  print(2.4)
  sigimp_dev_CLP <- approxfun(empty.dat.precip$SimDay, empty.dat.precip$Dev_CLP, rule = 2, method = "linear")
  
  #No burst vaccination should be done for latin hypercube analyses
  sigimp_vax  <- approxfun(All_Precip$SimDay, All_Precip$No_vax, rule = 2)
  
  #Run simulation
  print(3)
  start.run.time <- Sys.time()
  print(start.run.time)
  out <- as.data.frame(matrix.populations <- ode( y = pop_vec,
                                                  times = timestp,
                                                  func = ode.fun,
                                                  parms = param, #make sure this is a list
                                                  sigEndA = sigimpAMean,
                                                  sigC = sigimpCMean, 
                                                  sigdevA = sigimp_dev_ALP,
                                                  sigdevC = sigimp_dev_CLP, 
                                                  method = "ode45",
                                                  end.t = end.time
                                                  ))
  
  print(4)
  end.run.time <- Sys.time()
  print(end.run.time - start.run.time)
  flush.console()
  print(4.5)
  
  #Define years as MosqYears 
  out <- get.fun0(out, empty.dat.precip) #Define.MosqYr
  
  #Add the year to the data - now added by get.fun0
  end <- length(timestp)-1
  print(4.6)
  Yr<-head(empty.dat.precip, end)%>%
    select(Year)
  print(4.7)
  Yr<- rbind("1983", Yr)
  print(4.8)
  Yr<-as.data.frame(Yr)
  print(4.9)
  out <- cbind(Yr, out)
  print(4.95)
  out$Year<-as.numeric(out$Year)
  print(5)

#output of function
result <- param_matrix
print(6)
g <- length(param_matrix)
#Get the 6 outputs of interest
result[g + 1] <- get.fun1(out)#Get.Extinct.Day
print(7)
result[g + 2] <- get.fun2(out, param[["sigma_sl"]]) #Get.Max.Outbreak.Size.MosqYear#[,"Max.End.Size"]
print(8)
result[g + 3] <- get.fun3(out) #Get.Mean.SeroP.MosqY
print(9)
result[g + 4] <- get.fun4(out, param[["sigma_sl"]])# Get.Mean.Outbreak.Size.MosqYear
print(10)
result[g + 5] <- get.fun5(out)#Get.Mean.Outbreak.lengths.MosqYear
print(11)
result[g + 6] <- get.fun6(out)[,"end.prop.i"]#Get.Prop.Infected.Eggs - End day
print(12)

#Because get.fun0 added the year, it adds the initial conditions as 1983, but they are before 1983 so should be 1982
out$Year[1] <- 1982


#Save data from each simulation, uncomment if needed.
#FileNameC <- paste("Publication_Figures/Data_and_plots/SA/Run_", param[["Run"]], "_final_populations_data", file_date, ".csv", sep="")
#write.csv(out, FileNameC, row.names = FALSE)
#
empty.dat <- bind_rows(empty.dat, result)

return(empty.dat)

}