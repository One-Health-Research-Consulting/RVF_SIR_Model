###########
#' Author: Mindy Rostal
#' Title: Function for sensitivity analysis
#' date: April 10, 2022
#' ##################
#' 
#' Purpose: Create a function to conduct the sensitivity analysis and that can be used in an lapply command.
#' 
#' empty.dat must be as long as your parameter vector plus the output columns to add

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