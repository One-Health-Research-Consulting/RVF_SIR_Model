###############################
#Define hatching days for Aedes and Culex mosquitoes
#Mindy Rostal
#3-28-2022
###############################


#Load undefined hatching and climate data
All_Precip <- read.csv(here("All_Files_For_Publication", "Combined Temp and Precip Data for RVF Simulation.csv"))
#Functions for setting the hatching switches the data
source(here("All_Files_For_Publication/Functions", "Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R"))
#Source parameters
source(here("All_Files_For_Publication/Model_Scripts", "Model 1 - RVFV Selected Parameters.R"))


  #Define hatching days
  param_vec <- as.list(param_vec[1,])
  
  #Set up hatching - add columns the function will fill in
  All_Precip$hatchingA <- FALSE
  All_Precip$Days_rainyA <- 0# Has to get to 0 before hatching is permitted
  All_Precip$Days_hatchingA <- 0 # Hatching happens until this gets to CountDayRainA number of days
  All_Precip$hatchingC <- FALSE
  
  #Define days that pass the threshold for precipitation and temperature.
  All_Precip <- DayDefinition(All_Precip, 
                              rollcumrainA15 =param_vec$DaysCumRainA, 
                              cumrainC15 = param_vec$DaysCumRainC, 
                              minrainC = param_vec$MinRainC, 
                              minrainA = param_vec$MinRainA, 
                              Htemp = param_vec$TempAC)
  
  # Define the Aedes hatching days - MUST do Aedes days first!
  All_Precip <- AedesForcingEndemicMeanT(All_Precip, hatchabledayA = param_vec$DaysCanHatchA)
  
  #Define the Culex hatching days
  All_Precip <- CulexForcingMeanT(All_Precip, dayspostAhatch=param_vec$CulexHatchDelay)
  
  #Calculate development rates
  #Aedes
  All_Precip <- dev_ALP(dat = All_Precip, 
                        rh025 = dev.params.set["rh025a"], 
                        HA=dev.params.set["HAa"], 
                        TH=dev.params.set["THa"], 
                        HH=dev.params.set["HHa"])
  
  #Culex
  All_Precip <- dev_CLP(dat = All_Precip, 
                        rh025 = dev.params.set["rh025c"], 
                        HA= dev.params.set["HAc"], 
                        TH= dev.params.set["THc"], 
                        HH= dev.params.set["HHc"])
  
#Add vaccination pattern - none, daily, burst
  All_Precip <- All_Precip%>%
    mutate(No_vax = 0,
           Daily_vax = 1,
           Burst_vax = 0)
  
  
#Use approxfun to dictate if there is hatching on a daily timestep for use in the differential equations
#Hatching for Aedes 
sigimpAMean <- approxfun(All_Precip$SimDay, All_Precip$hatchingATMean, rule = 2)

# Hatching for Culex
sigimpCMean <- approxfun(All_Precip$SimDay, All_Precip$hatchingCPropTMean, rule = 2)

#Temp dependent development for Aedes and Culex
sigimp_dev_ALP <- approxfun(All_Precip$SimDay, All_Precip$Dev_ALP, rule = 2)
sigimp_dev_CLP <- approxfun(All_Precip$SimDay, All_Precip$Dev_CLP, rule = 2)

#Set vax pattern
if(Vaccinate == FALSE){
  sigimp_vax  <- approxfun(All_Precip$SimDay, All_Precip$No_vax, rule = 2)
}else{
  if(vax.burst == FALSE){
    sigimp_vax  <- approxfun(All_Precip$SimDay, All_Precip$Daily_vax, rule = 2)
  }else{
    for(y in vax.year){
      for(i in 1:nrow(All_Precip)){
        if(All_Precip$MosqYear[i] == y){
          if(All_Precip$MosqDay[i] >= 304 & All_Precip$MosqDay[i] <= 310)
            All_Precip$Burst_vax[i] <- 1
        }
      }
    }
    sigimp_vax  <- approxfun(All_Precip$SimDay, All_Precip$Burst_vax, rule = 2)
  }
}
