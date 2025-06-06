#Load packages:
library(codetools)
library(ggplot2)
library(deSolve)
library(zoo)
library(dplyr)
library(here)
library(lhs)
library(ggpubr)
library(egg)
library(stringr)
library(tidyr)
library(ggpattern)

#Set parameters
Check_Q <- FALSE
Check_Tasl <- FALSE
Check_Tsla <- TRUE
Check_epsilon <- FALSE
Check_Vax <- FALSE #Data for figure 4 RVFV persistence in host and aedes vs vaccine prop

#Always False:
SA <- FALSE #Sensitivity analysis, needs a different simulation file 
Var_Select <- FALSE #This should always be FALSE: If you want to run a sensitivity analysis, you need to use a different simulation file 
Vaccinate <- FALSE #Change to true is you want to run a simulation where you vaccinate the sheep and select a vaccination %
No.q <- FALSE #No transovarial transmission
Only.q <- FALSE #No horizontal transmission

if(Check_Vax == TRUE){
  Vaccinate <- TRUE #Need this true to get sigvax to do the daily vaccination rate - otherwise it will be 0 even if you put in a vax.prop value
}

set.seed(6242015)#This is to ensure the approxfun function always outputs the same mosquito hatching patterns

#Source other code
#Source assessment functions
source(here("All_Files_For_Publication/Functions", "Function 1 Define Functions for Output of Sensitivity Analysis.R"))
#Load ODE function
source(here("All_Files_For_Publication/Model_Scripts", "Model 3 RVFV ODE SIRS function.R"))
#Load plotting functions
source(here("All_Files_For_Publication/Functions", "Function 3 Plot simulations of a given time period.R"))
#Source climate dataframe - this sources "RVFV Optimized Parameters for publication.R" 
source(here("All_Files_For_Publication/Model_Scripts", "Model 2 Mosquito hatch rates at daily timestep.R"))

#Set parameter to check
if(Check_Q == TRUE){
  param <- "q"
}

if(Check_Tasl == TRUE){
  param <- "Tasl"
}
if(Check_Tsla == TRUE){
  param <- "Tsla"
}
if(Check_epsilon == TRUE){
  param <- "epsilon"
}
if(Check_Vax == TRUE){
  param <- "vax"
}

#Create vector of values to simulate, including the value for that parameter that is used in the model
vec <- seq(0,1, 0.05) #0.3

vec <- c(vec, param_vec[param])

if(Check_Vax== TRUE){
  Prop_vax_vec <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, .99)#proportion of hosts vaccinated
  #for vaccination calculations
  

  var2 <- "vax"
  var3 <- "g"
  var5 <- "muL"
  vec <- Prop_vax_vec*(as.numeric(param_vec[var5]) + as.numeric(param_vec[var3]))/ (1-Prop_vax_vec)
 #Two qs
  q_vec <- c(0.75, 0.9)
  
  #If we are looking at vaccine levels we need to hold the original starting values for R and S sheep and lambs to change it back at the end of the the vax loop
  origRS <- initial.populations[["RS"]]
  origSS <- initial.populations[["SS"]]
  origVS <- initial.populations[["VS"]]
  origRL <- initial.populations[["RL"]]
  origSL <- initial.populations[["SL"]]
  origVL <- initial.populations[["VL"]]
  origAL <- initial.populations[["AL"]]
  
}

#Set time
start.time <- 0
end.time <- tail(All_Precip$SimDay, 1) 
timestep <- 1
times <- seq(from=start.time, to=end.time, by=timestep)

#' Run the model
#' =============
start.run.time <- Sys.time()

print(start.run.time)


tab <- data.frame()

j <- 1
if(Check_Vax == FALSE){
for(i in vec){

  param_vec[param] <- i


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

final.populations <- as.data.frame(matrix.populations)
final.populations$time <- seq(1:length(final.populations$SS))

#Add the year
end <- length(final.populations$time)-1
Yr<-head(All_Precip, end)%>%
  select(Year)

  Yr<- rbind( Yr, "2017")

Yr<-as.data.frame(Yr)
final.populations <- cbind(Yr, final.populations)
final.populations$Year<-as.numeric(final.populations$Year)

final.populations <- Define.MosqYr(final.populations, All_Precip)
outbreak2010 <- which(final.populations$Year == 2010 & final.populations$MosqDay == 163) #February 10, 2010

#Make table of parameter value and outcomes
tab[j,1] <- Get.Mean.SeroP.MosqY(final.populations)
tab[j,2] <- Get.Extinct.Day(final.populations)
tab[j,3] <- param_vec[param]
tab[j,4] <- param

print(j)
j <- j+1 

}
}else{
  for(TOT in q_vec){
    param_vec["q"] <- TOT
  for(i in 1:length(vec)){
  
    param_vec[param] <- vec[i]
    param_vec["vax.prop"] <- Prop_vax_vec[i]
    
    #for(prop in Prop_vax_vec){
    param2_inv <- round((Prop_vax_vec[i]/(as.numeric(param_vec[var5]) + as.numeric(param_vec[var3]) + Prop_vax_vec[i])),2)
    NS = origRS + origSS + origVS
    initial.populations[["VS"]] <- (NS)*param2_inv
    initial.populations[["RS"]] <- if(origRS > (NS- NS*param2_inv)){
                                      (NS- NS*param2_inv)/2
                                    }else{
                                     origRS}
    initial.populations[["SS"]] <- NS - initial.populations[["RS"]] - initial.populations[["VS"]]
    NL = origRL + origSL + origVL + origAL
    initial.populations[["VL"]] <- (NL)*param2_inv
    initial.populations[["RL"]] <- origRL
    initial.populations[["AL"]] <- origAL
    initial.populations[["SL"]] <- NL - initial.populations[["RL"]] - initial.populations[["VL"]] - initial.populations[["AL"]] 
    

    
    
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
    
    final.populations <- as.data.frame(matrix.populations)
    final.populations$time <- seq(1:length(final.populations$SS))
    
    #Add the year
    end <- length(final.populations$time)-1
    Yr<-head(All_Precip, end)%>%
      select(Year)
    
    Yr<- rbind( Yr, "2017")
    
    Yr<-as.data.frame(Yr)
    final.populations <- cbind(Yr, final.populations)
    final.populations$Year<-as.numeric(final.populations$Year)
    
    final.populations <- Define.MosqYr(final.populations, All_Precip)
    outbreak2010 <- which(final.populations$Year == 2010 & final.populations$MosqDay == 163) #February 10, 2010
    
    #Make table of parameter value and outcomes
    tab[j,1] <- Get.Extinct.IAE.Day(final.populations, min.pop = 14400)[1]
    tab[j,2] <- Get.Extinct.Day(final.populations)
    tab[j,3] <- param_vec["vax.prop"]
    tab[j,4] <- Get.Extinct.IAE.Day(final.populations, min.pop = 14400)[2]
    tab[j,5] <- param_vec["q"]
    
    print(j)
    j <- j+1 
    
  }
  }
}

############################ Delete above
#Add column names
names(tab) <- c("Mean_Seroprevalence", "Extinction_Day", "Parameter_Value", "Parameter_Tested")
if(Check_Vax == TRUE){
  names(tab) <- c("Extinction_Day_in_Eggs", "Extinction_Day_in_Host", "Proportion_Vaccinated", "Last_Day_IAE_Over_14400", "q")
}

#Create file names
if(Check_Vax == FALSE){
data.file.title <- paste("./All_Files_For_Publication/Data_for_sensitivity_analysis/", param, " vs Mean Seroprevalence and Extinction Day.Rdata", sep = "")
csv.file.title <- paste("./All_Files_For_Publication/Data_for_sensitivity_analysis/", param, " vs Mean Seroprevalence and Extinction Day.csv", sep = "")
}else{
  data.file.title <- paste("./All_Files_For_Publication/Data_for_sensitivity_analysis/Percent ", param, " vs Persistence in host and vector.Rdata", sep = "")
  csv.file.title <- paste("./All_Files_For_Publication/Data_for_sensitivity_analysis/Percent ", param, " vs Persistence in host and vector.csv", sep = "")
}

#Save data files
save(tab, file = data.file.title )#Save the data
write.csv(tab, csv.file.title, row.names = FALSE)


#Make plots for a single parameter vs mean seroprevalence and persistence
if(Check_Vax == FALSE){
plot.SeroP <- ggplot(tab, aes(x = Parameter_Value, y = Mean_Seroprevalence))+
  geom_line()+
  labs(x = paste0("Value of ", param), y = "Mean_Seroprevalence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(colour = "black", size=12), 
        axis.text = element_text(colour = "black", size = 10))

plot.Extinct <- ggplot(tab, aes(x = Parameter_Value, y = Extinction_Day))+
  geom_line()+
  labs(x = paste0("Value of ", param), y = "Extinction_Day")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(colour = "black", size=12), 
        axis.text = element_text(colour = "black", size = 10))


Fig <- ggarrange(plot.SeroP, plot.Extinct, draw = FALSE,#
                  labels = c("A", "B"), #
                  ncol = 2, nrow = 1)
}else{
  if(Check_Vax == TRUE){
    tab.yr <- tab%>%
      mutate(Extinction_Yr_in_Host = round(Extinction_Day_in_Host/365,0))%>%
      mutate(Extinction_Yr_in_Eggs = round(Extinction_Day_in_Eggs/365,0))%>%
      pivot_longer(starts_with("Extinction_Yr"), names_to = "Species", values_to = "Years")%>%
      mutate(Last_Yr_IAE_Over_14400 = round(Last_Day_IAE_Over_14400/365,0))%>%
      mutate(Last_Yr_IAE_Over_14400 = replace(Last_Yr_IAE_Over_14400, Species == "Extinction_Yr_in_Host", 0))#%>%
      #mutate(Proportion_Vaccinated = Proportion_Vaccinated *100)

    unq_yr_vals <- unique(tab.yr$Last_Yr_IAE_Over_14400)
   
    unq_yr_vals <- unq_yr_vals[!is.na(unq_yr_vals)]
    
    no_yr <- NA
    list.pat <- c("Extinction_Yr_in_Eggs" = "stripe", "Extinction_Yr_in_Host" = "none")
    
    
##################################New
    tab.yr <- tab.yr%>%
      mutate(q_sp = paste(Species, q, sep = "_"))%>%
      mutate(Last_Yr_IAE_Over_14400 = if_else(Last_Yr_IAE_Over_14400 == 0 & str_detect(Species, "Host"), Years, Last_Yr_IAE_Over_14400))%>%
      mutate(q = if_else(q == 0.75, "Low", "High"))
    
    plot_vax_ipersist <- ggplot(tab.yr, aes(x = Proportion_Vaccinated)) +
      geom_bar_pattern(aes(y = Last_Yr_IAE_Over_14400, pattern = q, fill = Species), 
                       stat = "identity",
                       position = "dodge",
                       color = "black", 
                       show.legend = TRUE,
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
      scale_x_continuous(labels = scales::percent) +
      scale_pattern_manual(values = c("Low" = "stripe", "High" = "none"), labels = sort(unique(tab.yr$q)), name = "Transovarial transmission \n fraction") +
      scale_fill_brewer(palette="Set2", name = "Extinction Year", labels = expression(paste( italic("Aedes"), " Eggs"), paste("Host"))) +
      labs(x = "Percent Vaccinated", y = "Year of Extinction") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.title = element_text(colour = "black", size=18), 
            axis.text = element_text(colour = "black", size = 14)) + 
      guides(pattern = guide_legend(override.aes = list(fill = "white")),
             fill = guide_legend(override.aes = list(pattern = "none"))) 
    plot_vax_ipersist
    
  Fig <- ggarrange(plot_vax_ipersist, draw = FALSE,
                   ncol = 1, nrow = 1)
  }
}

if(Check_Vax == FALSE){
fil.name <- paste0("./All_Files_For_Publication/Data_for_sensitivity_analysis/Fig of ", param, " vs mean seroprevalence and persistence.png")
ggexport(Fig, filename = fil.name, width=522, height=305, ncol = 2,nrow = 1)
}else{
  fil.name <- paste0("./All_Files_For_Publication/Data_for_sensitivity_analysis/Fig 4d of percent ", param, " vs persistence in host and vector.pdf")
  ggexport(Fig, filename = fil.name, width=7, height=3, ncol = 1,nrow = 1)
  }

