#' ---
#' title: "Automatically run all files in order to output all figures and data tables for the manuscript"
#' author: "Mindy Rostal"
#' date: "June 6, 2025"
#' output: html_document
#' ---
#' 
#' This will run all of the different simulation scenarios except some of the R0 and sensitivity analysis results.
#Load packages:
library(dplyr)
library(egg)
library(codetools)
library(ggplot2)
library(deSolve)
library(zoo)
library(here)
library(ggpubr)
library(stringr)

No.q <- FALSE #No transovarial transmission
Only.q <- FALSE #No horizontal transmission
muC.25_higher <- FALSE #Run simulation with a Culex mortality rate that is 25% higher
muC.25_lower <- FALSE #Run simulation with a Culex mortality rate that is 25% lower
lo.eggs <- FALSE #Run simulation that changes the initial population of infected Aedes eggs
no.amp.vecs <- FALSE # Run simulation that does not allow Culex to transmit the virus
Vaccinate <- FALSE
vax.proportion <- .18 #Proportion of the flock user wants to remain vaccinated at a constant rate
vax.25_higher <- FALSE #Run simulation with a host vaccination rate that is 3 x higher
vax.25_lower <- FALSE
#Run simulation with a host vaccination rate that is 25% higher

#Always False:
SA <- FALSE #This should always be FALSE: If you want to run a sensitivity analysis, you need to use a different simulation file 
Var_Select <- FALSE  #This should always be FALSE: If you want to run a sensitivity analysis, you need to use a different simulation file 

set.seed(6242015)#This is to ensure the approxfun function always outputs the same mosquito hatching patterns

#Doesn't include lo.eggs.
vector_of_scenarios <- c("No.q", "Only.q", "muC.25_higher", "muC.25_lower",  "no.amp.vecs", "Exemplar")

#Source code
#Source functions
source(here("All_Files_For_Publication/Functions", "Function 1 Define Functions for Output of Sensitivity Analysis.R"))
#Load ODE function
source(here("All_Files_For_Publication/Model_Scripts", "Model 3 RVFV ODE SIRS function.R"))
#Load plotting functions
source(here("All_Files_For_Publication/Functions", "Function 3 Plot simulations of a given time period.R"))
#Source integrated hatching data for each timestep - parameter file is sourced by the climate/mosquito hatching code
source(here("All_Files_For_Publication/Model_Scripts", "Model 2 Mosquito hatch rates at daily timestep.R"))
#Source Reffective fuctions
source(here("All_Files_For_Publication/Functions", "Function 4 Calculate R0.R"))
source(here("All_Files_For_Publication/Functions", "Function 5 Calculate R0 dfs for plot.R"))



##################
#Run all as a for loop
############################
#############################
#For vaccination scenarios after we go through the final non-vaccine scenario
lvec <- length(vector_of_scenarios)
if(vector_of_scenarios[lvec] == "Exemplar"){
  Vaccinate <- TRUE
  vector_of_vax_scenarios <- c("vax.25_higher", "vax.25_lower", "vax")
}

if(Vaccinate == TRUE){
  for(vax_param in vector_of_vax_scenarios){
    vax.25_higher <- FALSE
    vax.25_lower <- FALSE
    vax <- FALSE
    #Source functions
    source(here("All_Files_For_Publication/Functions", "Function 1 Define Functions for Output of Sensitivity Analysis.R"))
    #Load ODE function
    source(here("All_Files_For_Publication/Model_Scripts", "Model 3 RVFV ODE SIRS function.R"))
    #Load plotting functions
    source(here("All_Files_For_Publication/Functions", "Function 3 Plot simulations of a given time period.R"))
    #Source integrated hatching data for each timestep - parameter file is sourced by the climate/mosquito hatching code
    source(here("All_Files_For_Publication/Model_Scripts", "Model 2 Mosquito hatch rates at daily timestep.R"))
    #Source Reffective fuctions
    source(here("All_Files_For_Publication/Functions", "Function 4 Calculate R0.R"))
    source(here("All_Files_For_Publication/Functions", "Function 5 Calculate R0 dfs for plot.R"))
    # }
    #Set parameters for various scenarios if they are selected
    if(vax_param == "vax.25_higher"){
      param_vec["vax.prop"] <- vax.proportion * 3 # actually 54% vaccinated, which is much higher than just 25% higher
      vax <- param_vec[["vax.prop"]]*(param_vec[["muL"]] + param_vec[["g"]])/ (1-param_vec[["vax.prop"]]) #vax = p*(muL+g)/(1-p)
      param_vec["vax"] <- vax
      vax.25_higher <- TRUE
    }
    if(vax_param == "vax.25_lower"){
      param_vec["vax.prop"] <- vax.proportion * 0.750
      vax <- param_vec[["vax.prop"]]*(param_vec[["muL"]] + param_vec[["g"]])/ (1-param_vec[["vax.prop"]]) #vax = p*(muL+g)/(1-p)
      param_vec["vax"] <- vax
      vax.25_lower <- TRUE
    }
    if(vax_param == "vax"){#Just vaccinate
      param_vec["vax.prop"] <- vax.proportion
      vax <- param_vec[["vax.prop"]]*(param_vec[["muL"]] + param_vec[["g"]])/ (1-param_vec[["vax.prop"]]) #vax = p*(muL+g)/(1-p)
      param_vec["vax"] <- vax
    }
    
    
    #Run model
    #Set time
    start.time <- 0
    end.time <- tail(All_Precip$SimDay, 1) #3000#
    timestep <- 1
    times <- seq(from=start.time, to=end.time, by=timestep)
    
    ##################
    #' Run the model
    #' =============
    start.run.time <- Sys.time()
    
    print(start.run.time)
    
    matrix.populations <- ode(y = initial.populations,
                              times = times,
                              func = ODE.RVFV.deterministic.SIRS,
                              parms = param_vec, #make sure this is a list
                              sigEndA = sigimpAMean,
                              sigC = sigimpCMean, 
                              sigdevA = sigimp_dev_ALP,
                              sigdevC = sigimp_dev_CLP,
                              end.t = end.time
    )
    
    End.run.time <- Sys.time()
    print(End.run.time - start.run.time)
    
    #Make into data frame and add time column
    final.populations <- as.data.frame(matrix.populations)
    final.populations$time <- seq(1:length(final.populations$SS))
    
    #Add the year
    end <- length(final.populations$time)-1
    Yr<-head(All_Precip, end)%>%
      select(Year)
    Yr<- rbind( Yr, "2017")#one additional day - is in 2017
    Yr<-as.data.frame(Yr)
    
    final.populations <- cbind(Yr, final.populations)
    final.populations$Year<-as.numeric(final.populations$Year)
    
    #Define hatching days
    final.populations <- Define.MosqYr(final.populations, All_Precip)
    #Set outbreak parameter to mark 2010 outbreak on the timeline
    outbreak2010 <- which(final.populations$Year == 2010 & final.populations$MosqDay == 163) #February 10, 2010
    
    #Plot and save
    source("./All_Files_For_Publication/Analysis/Analysis_1 Code for multiplots.R")
    
    print(vax_param)
    
  }
  Vaccinate <- FALSE
}

###############################
##############################


  #All scenarios but vaccination
for(param_change in vector_of_scenarios){

  #Set all scenarios to FALSE
  #Set Scenarios  - User decides at the start
  No.q <- FALSE #No transovarial transmission
  Only.q <- FALSE #No horizontal transmission
  muC.25_higher <- FALSE #Run simulation with a Culex mortality rate that is 25% higher
  muC.25_lower <- FALSE #Run simulation with a Culex mortality rate that is 25% lower
  lo.eggs <- FALSE #Run simulation that changes the initial population of infected Aedes eggs
  no.amp.vecs <- FALSE # Run simulation that does not allow Culex to transmit the virus
  Vaccinate <- FALSE #Change to true is you want to run a simulation where you vaccinate the sheep and select a vaccination %
  
  #Reset the paramters
  #Source integrated hatching data for each timestep - parameter file is sourced by the climate/mosquito hatching code
  source(here("All_Files_For_Publication/Model_Scripts", "Model 2 Mosquito hatch rates at daily timestep.R"))
  
  
#No transovarial transmission
if(param_change == "No.q"){
  param_vec["q"] <- 0.0
  No.q <- TRUE
}

#No horizontal transmission among any vectors
if(param_change == "Only.q"){
  param_vec["Tcsl"] <- 0
  param_vec["Tslc"] <- 0
  param_vec["Tasl"] <- 0
  Only.q <- TRUE
}

#No Culex transmission - only Aedes, which have vertical and horizontal
if(param_change == "no.amp.vecs"){
  param_vec["Tcsl"] <- 0
  param_vec["Tslc"] <- 0
  no.amp.vecs <- TRUE
}

#low initial Aedes egg starting population
if(param_change == "lo.eggs"){
  initial.populations["IAE"] <- (initial.populations["SAE"] + initial.populations["IAE"]) * 0.0003 #main model initial pop is pop.sizeAE * 0.003
  lo.eggs <- TRUE
}

#Culex mortality rate
if(param_change == "muC.25_higher"){
  param_vec["muC"] <- param_vec[["muC"]] * 1.25
  muC.25_higher <- TRUE
}

if(param_change == "muC.25_lower"){
  param_vec["muC"] <- param_vec[["muC"]] * 0.75
  muC.25_lower <- TRUE
}

  if(param_change == "Exemplar"){
    #No changes, everything is FASLE
  }  
  

#Run model
  #Set time
  start.time <- 0
  end.time <- tail(All_Precip$SimDay, 1) #3000#
  timestep <- 1
  times <- seq(from=start.time, to=end.time, by=timestep)
  
  ##################
  #' Run the model
  #' =============
  start.run.time <- Sys.time()
  
  print(start.run.time)
  
  matrix.populations <- ode(y = initial.populations,
                            times = times,
                            func = ODE.RVFV.deterministic.SIRS,
                            parms = param_vec, #make sure this is a list
                            sigEndA = sigimpAMean,
                            sigC = sigimpCMean, 
                            sigdevA = sigimp_dev_ALP,
                            sigdevC = sigimp_dev_CLP,
                            end.t = end.time
  )
  
  End.run.time <- Sys.time()
  print(End.run.time - start.run.time)
  
  #Make into data frame and add time column
  final.populations <- as.data.frame(matrix.populations)
  final.populations$time <- seq(1:length(final.populations$SS))
  
  #Add the year
  end <- length(final.populations$time)-1
  Yr<-head(All_Precip, end)%>%
    select(Year)
  Yr<- rbind( Yr, "2017")#one additional day - is in 2017
  Yr<-as.data.frame(Yr)
  
  final.populations <- cbind(Yr, final.populations)
  final.populations$Year<-as.numeric(final.populations$Year)
  
  #Define hatching days
  final.populations <- Define.MosqYr(final.populations, All_Precip)
  #Set outbreak parameter to mark 2010 outbreak on the timeline
  outbreak2010 <- which(final.populations$Year == 2010 & final.populations$MosqDay == 163) #February 10, 2010
 
  #Get outbreak dataframe
  final.populations <- final.populations%>%
    mutate(NSL = NS + NL)%>%
    mutate(All_AEggs = SAE + IAE)
  
  Outbreaks <- Get.Outbreak.Dataframe(final.populations, param_vec$sigma_sl)
  ##### 
  #For use in the R0 analyses 
  #Calculate the mean dev_ALP, dev_CLP, mu_CLP and mu_ALP only from days when these factors are non-zero
  ALP_rates <- final.populations%>%
    filter(SALP >=1 | IALP >=1)
  CLP_rates <- final.populations%>%
    filter(SCLP >=1)
  
  mdev_ALP <- mean(ALP_rates$dev_ALP) 
  mmu_ALP <- mean(ALP_rates$muALP) 
  
  mdev_CLP <- mean(CLP_rates$dev_CLP) 
  mmu_CLP <- mean(CLP_rates$muCLP) 
  
  #Get peak infected populations
  PeakInfectedMosq <- InfectedMosqPeak(final.populations)
  
  PeakInfectedMosq <- full_join(PeakInfectedMosq, Outbreaks)
  
  
  ###################################
  #Add Effective R0 to final.populations
  R_pops <- select(final.populations, SS, SL, SA, SC, MosqYear)
  
  Reff.list <- apply(R_pops, 1, function(x) R_eff(x,  params = R0params, get.fun1 = calc_R0))
  
  final.populations$Reff <- Reff.list  
  
  #Plot and save
  source("./All_Files_For_Publication/Analysis/Analysis_1 Code for multiplots.R")
  
  print(param_change)
}

 
############
#Figures made using different files
  #Fig 3
  source("./All_Files_For_Publication/Analysis/Analysis_4 Assessing the sensitivity analysis.R")
  print("Fig 3 completed")  

  # Fig S4 and S5
  source("./All_Files_For_Publication/3- Calculate and Plot_R0.R")
  print("Fig S4 and S5 completed")
  
  #S13
  source("./All_Files_For_Publication/Analysis/Analysis_0 Select from param space exploration.R")
  print("Fig S13 compltee")
      


    
    

    
 
  


  