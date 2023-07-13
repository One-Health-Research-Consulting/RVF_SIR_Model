#' Title: Assessment of sensitivity analysis
#' Author: Mindy Rostal
#' Date: 5/3/2022
#' 
#' 
#' Purpose: To evaluate the effect that varying the selected parameters have on the outputs of the sensitivity analysis.

#'Load Packages
library(here)
library(dplyr)
library(ggplot2)
library(stringr)
library(zoo)
library(sensitivity)
library(data.table)
library(tibble)
library(ggpubr)
library(egg)


#'Load data produced from the 2_RVF_LHC_Sensitivity_Analysis.R file
load(here("./All_Files_For_Publication/Data_for_sensitivity_analysis/", "SA_trans_Publication2023_07_10.Rdata"))

#Set ggplot theme
plot_theme <- theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.title = element_text(size = rel(1.5))) + 
  theme(legend.text = element_text(size = 10)) + 
  theme(plot.title = element_text(size=16))

SA_trans <- data

#Set seed for random selection
set.seed(623)

#Name according to date run
tname <- "2021_05_07_scaled_params" #Date of analysis


#Set alpha
bonferroni.alpha <- 0.05/12

#Calculate the proportion of simulations that go extinct or that have a very high seroprevalence and the proportion that are within our target seroprevalece and persist.
Extinct.by.last.yr <- max(SA_trans$Extinction.Day)-365
Prct.Extinct <- length(which(SA_trans$Extinction.Day<Extinct.by.last.yr))/nrow(SA_trans)*100
Prct.Hi.Serop <- length(which(SA_trans$Mean.Annual.Seroprevalence>.40))/nrow(SA_trans)*100
prct.In.Our.Zone <- length(which(SA_trans$Mean.Annual.Seroprevalence<=.40 & SA_trans$Extinction.Day>=Extinct.by.last.yr))/nrow(SA_trans)*100


#plot the simulations
ParamSp <- SA_trans%>%
  mutate(Extinct.yr = Extinction.Day/365)%>%
  mutate(Persist = if_else(Extinction.Day>Extinct.by.last.yr, "Yes", "No"))%>%
  mutate(SeroPZone = if_else(Mean.Annual.Seroprevalence<=.40 & Extinction.Day>=Extinct.by.last.yr, "Yes", "No"))%>%
  mutate(Mean.Annual.Seroprevalence = round(Mean.Annual.Seroprevalence*100,2))



ParamSp <- ParamSp%>%
  mutate(InParamSpace = if_else(Persist == "Yes" & SeroPZone == "Yes", "Yes", "No"))


yes <- ParamSp%>%
  filter(InParamSpace == "Yes")

ggplot(ParamSp, aes(x = Mean.Annual.Seroprevalence, y = Extinct.yr) ) +
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +
  geom_jitter(data = yes, aes(x = Mean.Annual.Seroprevalence, y = Extinct.yr),colour = "red") +
  plot_theme +
  labs(x = "Mean Seroprevalence (%)",
     y = "Years of Persistence",
     fill = "Number of \nSimulations")

#ggplot(data = yes, aes(x = Mean.Annual.Seroprevalence, y = Extinct.yr))+geom_point(colour = "red")

#,colour = "red" )


##########################################
#List parameter vectors that were varied during analysis and output factors
#######################################################

#Define vectors
#Parameters to be evaluated from sensitivity analysis
Variable_Set <- c("q", "biteA",  "Tsla", "Tasl", "muA", "epsilon",  "muAE",  
                  "biteC", "Tslc", "Tcsl", "muC", 
                  "rhoS", "rhoL", "sigma_sl" )
Variable_Names <- cbind("q" = "Transovarial Transmission", "biteA" = "Aedes Bite Rate", "Tsla" = "Transmission Rate from Aedes to Host", 
                        "Tasl" = "Transmission Rate from Host to Aedes", "muA" = "Aedes Mortality Rate", "epsilon" = "Extrinsic Incubation", 
                        "muAE" = "Mortality Rate of Aedes Eggs", 
                        "biteC" = "Culex Bite Rate", "Tslc" = "Transmission Rate from Culex to Host", "Tcsl" = "Transmission Rate from Host to Culex", 
                        "muC" = "Culex Mortality Rate", 
                        "rhoS" = "Sheep Specific RVFV Mortality Rate", "rhoL" = "Lamb Specific RVFV Mortality Rate", "sigma_sl" = "Host RecoveryRrate")

Variable_Names <- as.data.frame(Variable_Names)
Variable_Names <- mutate_all(Variable_Names, list(~as.character(.)))

#Output factors we are reviewing
Output_Set <- c("Extinction.Day", "Mean.Annual.Seroprevalence",  "Get.Mean.Outbreak.Size.MosqYear", "Max.Outbreak.Size.MosqYear", "Get.Mean.Outbreak.lengths.MosqYear", "Get.Prop.Infected.Eggs_last_day") 


###################################################
#Sesnsitivity analysis
###################################################
#Select columns  
SA <- SA_trans%>%
    select(q, Tslc, Tcsl, Tsla, Tasl, biteA, muA, epsilon, muAE, biteC, muC, rhoS, rhoL, sigma_sl, Extinction.Day, Mean.Annual.Seroprevalence,  Get.Mean.Outbreak.Size.MosqYear, Max.Outbreak.Size.MosqYear, Get.Mean.Outbreak.lengths.MosqYear, Get.Prop.Infected.Eggs_last_day) #
  
#Set Class
SA <- SA%>%
  mutate_at(vars(Extinction.Day , Get.Mean.Outbreak.lengths.MosqYear), list(~as.numeric(.)))

#'########################################
#'Partial Coefficient Correlation analysis
#'########################################

  #Create an empty list
  pcc <- list()
  #Set the first 9 columns of the data set to be equal to the first 14 columns of the SA data.frame
  dat <- as.data.frame(SA[,1:14])
  
  #'Run loop to calculate PCC
  for(out in Output_Set){#Function for output set vector
    var_vec <- SA[[out]]#Create a vector ofust out output factor of interest
    Corr <- pcc(dat, var_vec, nboot = 1000, rank=TRUE, conf=1-bonferroni.alpha)
    #Calculates the partial coeffcient correlations for the parameter that was varied and are in the dat against the vector that I created.
    pcc[[out]] <- Corr$PRCC#Add the output of the PCC analysis to a list - one item of the list per output factor
  }

#Create table 1
  Estimate.Table <- data.frame()
  for(i in 1:length(pcc)){
    a <- pcc[[i]]
    nom <- names(pcc[i])
    a <- as.data.frame(a)
    PCCTab <- data.frame()
    #Put them into logical order
    PCCTab <- rbind(PCCTab, a[1,]) # Transovarial Transmission first
    PCCTab <- rbind(PCCTab, a[6,]) # Aedes Bite Rate
    PCCTab <- rbind(PCCTab, a[4,]) # Trans A to Rum
    PCCTab <- rbind(PCCTab, a[5,]) # Trans Rum to A
    PCCTab <- rbind(PCCTab, a[7,]) # Adult Aedes death rate
    PCCTab <- rbind(PCCTab, a[8,]) # Extrinsic Incubation
    PCCTab <- rbind(PCCTab, a[9,]) # Egg death rate
    PCCTab <- rbind(PCCTab, a[10,])# Culex Bite Rate
    PCCTab <- rbind(PCCTab, a[2,]) # Trans C to Rum
    PCCTab <- rbind(PCCTab, a[3,]) # Trans Rum to C
    PCCTab <- rbind(PCCTab, a[11,])# Adult Culex death rate
    PCCTab <- rbind(PCCTab, a[12,])# Sheep specific RVF mortality
    PCCTab <- rbind(PCCTab, a[13,])# Lamb specific RVF mortality
    PCCTab <- rbind(PCCTab, a[14,])# Recovery Rate

    #Extract pertinent information for the table
    PCCTab$OutputFactor <- nom
    name.list <- c("Estimate", "bias", "SE", "MinCI", "MaxCI", "OutputFactor")
    names(PCCTab)<- name.list
    PCCTab <- PCCTab %>%
      rownames_to_column("Variable")%>%
      mutate(CI = paste("[", round(MinCI, 3), ", ", round(MaxCI,3), "]", sep = ""))%>%
      mutate(Sig = if_else(between(0, MinCI, MaxCI), "no", "*"))%>%
      mutate(CI = if_else(Sig == "*", paste(CI, "*", sep = ""), CI))%>%
      mutate(Estimate = round(Estimate, 3))%>%
      select(Variable, OutputFactor, Estimate, CI)%>%
      column_to_rownames("Variable")
    if(i == 1){
      Estimate.Table <- rbind(Estimate.Table, PCCTab)
    }else{
      Estimate.Table <- cbind(Estimate.Table, PCCTab)
    }  
  }
  
  Parameter<-rownames(Estimate.Table)
  
  Estimate.Table <- cbind(Parameter, Estimate.Table)
  rownames(Estimate.Table) <- NULL#rownames_to_column(Estimate.Table, "Parameter")
  
  #Write table 
  write.csv(Estimate.Table, here("Publication_Figures", "Table 1 Estimate of PCCs_for Publication.csv"), row.names = FALSE)

