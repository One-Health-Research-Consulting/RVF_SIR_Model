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
library(tidyr)


#'Load data produced from the 2_RVF_LHC_Sensitivity_Analysis.R file
load(here("./All_Files_For_Publication/Data_for_sensitivity_analysis/", "SA_trans_Publication2025_02_10.Rdata"))

#Set ggplot theme
plot_theme <- theme_classic() +
  theme(text = element_text("serif"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = 10),
        plot.title = element_text(size=16))

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
Output_Set <- c("Extinction.Day", "Mean.Annual.Seroprevalence", "Get.Prop.Infected.Eggs_last_day", "Get.Mean.Outbreak.lengths.MosqYear",  "Get.Mean.Outbreak.Size.MosqYear", "Max.Outbreak.Size.MosqYear") 


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

  #Data for tornado plot
  #Create table 1
  Torn_Table <- data.frame()
  Torn_Table2 <- data.frame()
  for(i in 1:length(pcc)){
    a <- pcc[[i]]
    nom <- names(pcc[i])
    a <- as.data.frame(a)
    PCCplot <- data.frame()
    #Put them into logical order
    PCCplot <- rbind(PCCplot, a[1,]) # Transovarial Transmission first
    PCCplot <- rbind(PCCplot, a[6,]) # Aedes Bite Rate
    PCCplot <- rbind(PCCplot, a[4,]) # Trans A to Rum
    PCCplot <- rbind(PCCplot, a[5,]) # Trans Rum to A
    PCCplot <- rbind(PCCplot, a[7,]) # Adult Aedes death rate
    PCCplot <- rbind(PCCplot, a[8,]) # Extrinsic Incubation
    PCCplot <- rbind(PCCplot, a[9,]) # Egg death rate
    PCCplot <- rbind(PCCplot, a[10,])# Culex Bite Rate
    PCCplot <- rbind(PCCplot, a[2,]) # Trans C to Rum
    PCCplot <- rbind(PCCplot, a[3,]) # Trans Rum to C
    PCCplot <- rbind(PCCplot, a[11,])# Adult Culex death rate
    PCCplot <- rbind(PCCplot, a[12,])# Sheep specific RVF mortality
    PCCplot <- rbind(PCCplot, a[13,])# Lamb specific RVF mortality
    PCCplot <- rbind(PCCplot, a[14,])# Recovery Rate
    
    #Extract pertinent information for the table
    PCCplot$OutputFactor <- nom
    name.list <- c("mean", "bias", "SE", "Lower", "Upper", "OutputFactor")
    names(PCCplot)<- name.list
    PCCplot <- PCCplot %>%
      rownames_to_column("variable")%>%
      mutate(tot_dif = if_else(Upper < 0 & Lower < 0, Lower - Upper, if_else(Upper > 0 & Lower > 0, Upper - Lower, abs(Upper) + abs(Lower))))%>%
      mutate(tot_dif = abs(tot_dif))%>%
      arrange(tot_dif)
    
    PCCplot2 <- PCCplot %>%
      pivot_longer(cols = Lower:Upper, names_to = "level",  values_to = "value")%>%
      select(OutputFactor, variable, level, value)
    Torn_Table <- rbind(Torn_Table, PCCplot)  
    Torn_Table2 <- rbind(Torn_Table2, PCCplot2)
  }
  
  #Torn_Table$variable <- factor( Torn_Table$variable, levels = c("q", "biteA", "muA", "Tsla", "sigma_sl", "Tasl", "epsilon", "biteC", "muC", "rhoS", "rhoL", "Tcsl", "Tslc", "muAE"))
  
  Torn_Table2$variable <- factor( Torn_Table2$variable, levels = c("muAE", "Tslc", "Tcsl", "rhoL", "rhoS",  "biteC", "muC", "epsilon", "Tasl", "sigma_sl", "Tsla", "muA", "biteA", "q"))
  Torn_Table$variable <- factor( Torn_Table$variable, levels = c("muAE", "Tslc", "Tcsl", "rhoL", "rhoS",  "biteC", "muC", "epsilon", "Tasl", "sigma_sl", "Tsla", "muA", "biteA", "q"))
  
  ggplot(Torn_Table2, aes(variable, value, fill=level)) +
    geom_bar(position="identity", stat="identity") +
    facet_grid( ~ OutputFactor) +
    coord_flip() +
    plot_theme
  
  #Italics titles
  y_x_title.Abite <- expression(paste(italic("Aedes"), " Bite Rate"))
  y_x_title.Tsla  <- expression(paste("Transmission from ", italic("Aedes"), " to Host")) 
  y_x_title.Tasl  <- expression(paste("Transmission from Host to ", italic("Aedes"))) 
  y_x_title.muA   <- expression(paste(italic("Aedes"), " Mortality Rate")) 
  y_x_title.muAE  <- expression(paste("Mortality Rate of ", italic("Aedes"), " Eggs")) 
  y_x_title.biteC <- expression(paste(italic("Culex"), " Bite Rate"))
  y_x_title.Tslc  <- expression(paste("Transmission from ", italic("Culex"), " to Host"))
  y_x_title.Tcsl  <- expression(paste("Transmission from Host to ", italic("Culex")))
  y_x_title.muC   <- expression(paste(italic("Culex"), " Mortality Rate"))
  
  #Facet labels
 facet.lab <- c("Extinction.Day" = "Persistence", "Mean.Annual.Seroprevalence" = "Mean Seroprevalence \nAcross All Years", "Get.Prop.Infected.Eggs_last_day" = "Proportion of Infected Eggs \nat End of Simulation", "Get.Mean.Outbreak.lengths.MosqYear" = "Mean Outbreak Length",  "Get.Mean.Outbreak.Size.MosqYear" = "Mean Outbreak Size", "Max.Outbreak.Size.MosqYear" = "Maximum Single \nOutbreak Size")
  
  
 ggplot(Torn_Table2, aes( x = variable, y = value, fill=level)) +
   geom_bar(position="identity", stat="identity") 
 
  Fig3Tornado <- ggplot() +
    geom_bar(data = Torn_Table2, aes( x = variable, y = value, fill = "lightblue"), stat = "identity", position = "identity") +
    geom_boxplot(data = Torn_Table,
      stat = "identity",
      aes(ymin  = Lower,
          ymax  = Upper,
          middle = mean, 
          lower = Lower,
          upper = Upper,
          y = mean,
          x = variable)) +
    scale_fill_manual( values = "lightblue")+
    scale_color_manual( values = "lightblue") +
    scale_x_discrete(labels=c("q" = "Transovarial Transmission", 
                              "biteA" = y_x_title.Abite, 
                              "Tsla" = y_x_title.Tsla, 
                              "Tasl" = y_x_title.Tasl, 
                              "muA" = y_x_title.muA, 
                              "epsilon" = "Extrinsic Incubation", 
                              "muAE" = y_x_title.muAE, 
                              "biteC" = y_x_title.biteC, 
                              "Tslc" = y_x_title.Tslc, 
                              "Tcsl" = y_x_title.Tcsl, 
                              "muC" = y_x_title.muC, 
                              "rhoS" = "Sheep Specific RVFV Mortality Rate", "rhoL" = "Lamb Specific RVFV Mortality Rate", "sigma_sl" = "Host Recovery Rate")) +
  
    geom_boxplot(data = Torn_Table,
      stat = "identity",
      aes(ymin  = Lower,
          ymax  = Upper,
          middle = mean, 
          lower = Lower,
          upper = Upper,
          y = mean,
          x = variable),  color = "darkblue", fill = NA, fatten = 0) +
    facet_wrap(~OutputFactor, labeller = labeller(OutputFactor = facet.lab), ncol = 3) +
    labs(x = "Parameter", y = "Estimate") +
    coord_flip() +
    plot_theme +
    theme(legend.position = "blank",
          plot.margin = margin(1,1,1.5,1.2, "cm"),
          strip.text.x = element_text(size = 7),
          axis.text = element_text(size = 8, colour = "black"))
  
  Fig3Tornado
  
  Fig3 <- ggarrange(Fig3Tornado, draw = FALSE,
                    ncol = 1, nrow = 1)
  
  fil.namefig3.pdf <- "./Publication_Figures/Fig 3 Tornado Plot of sensitivity analysis results.pdf"
 
  
  ggexport(Fig3, filename = fil.namefig3.pdf, width=11, height= 6)

  