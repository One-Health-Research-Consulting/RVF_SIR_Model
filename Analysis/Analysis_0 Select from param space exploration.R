library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(egg)
library(ggpubr)
library(grid)
library(png)


#################
# To obtain these datafiles, a Latin hypecube was run, see file "0_RVF_LHC_to_select_params.R"
# that only varied the 6 parameters that do not have good estimates. 

#Set ggplot theme
plot_theme <- theme_classic() +
  theme(axis.text = element_text(size = 5.5, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        plot.title = element_text(size=16))

#Obtain list of file names of datafiles we want to read in
files <- list.files("./All_Files_For_Publication/Data_for_sensitivity_analysis/results_20221024", pattern="*.Rdata", full.names=TRUE)

for (i in 1:length(files)){
  
  #Load each file and extract info from file name to insert into column called "file
  load(files[i])
  name <- str_remove(files[i], "results-newest/data_h4000_")
  name <- str_remove(name, ".Rdata")
  name <- str_remove(name, "_2022")
  name <- gsub("^i[^_]+_", "", name) 
  data$dataset <- i
  data$file <- name
  
  #Stick data together into single file
  # Create the first data if no data exist yet
  if (i == 1){
    new_data <- data
  }
  # if data already exist, then append it together
  if (i > 1){
    temp <- data
    new_data <- rbind(new_data, temp)
    rm(temp)
  }
}
head(new_data)
tail(new_data)
################################################################################

data <- new_data 

days_per_year <- 365
data2 <-  data %>% select(dataset, file, Run, muAE, NAEmax, biteA, NCEmax, biteC, 
                          Extinction.Day, Mean.Annual.Seroprevalence, q,
                          Get.Mean.SeroP.MosqY1, Get.Mean.SeroP.MosqY2, Get.Mean.SeroP.MosqY3,
                          Get.test1, Get.test2, Get.test3, 
                          Get.End.SeroP, spike_detect,
                          Get.Maxmax.IAE, Get.Minmax.IAE, Get.Maxmax.IC, Get.Minmax.IC,Get.Maxmax.IA, Get.Minmax.IA) %>%
  rename(ED = Extinction.Day, MAS = Mean.Annual.Seroprevalence) %>% 
  mutate(eggs21 = Get.test2/Get.test1, 
         eggs31 = Get.test3/Get.test1, 
         eggs32 = Get.test3/Get.test2, 
         SeroP1 = Get.Mean.SeroP.MosqY1,
         SeroP2 = Get.Mean.SeroP.MosqY2, 
         SeroP3 = Get.Mean.SeroP.MosqY3,
         ratioP = SeroP3/SeroP1, 
         ratioE = eggs31, 
         outbreak = Get.Maxmax.IC/Get.Maxmax.IA, 
         ED = ED/days_per_year,
         SD = spike_detect) %>%
  unite(Run_id, c(file, Run), sep = "_") %>%
  select(muAE, NAEmax, biteA, NCEmax, biteC, q, ratioP, ratioE, outbreak, ED, MAS, SD)

head(data2)
################################################################################
#Filter data for criteria
#Criteria used for the selected parameters (my_criteria):
    #Mean Seroprevalence between 5-40%
    #Persistence for at least 34 years
    #At least 1 spike detected (they all have at least 2 detected)
    #ratioE is between 0.9-1.1 (so the Aedes eggs remain pretty constant over the period)
    #ratioP is between 0.9-1.1 (so the mean seroprevalence remains pretty constant over the period)
nrow(data2)
filtered_data <- data2 %>% filter(MAS < 0.4 & MAS > 0.05, 
                                  ED > 34, 
                                  ratioE > 0.9 & ratioE < 1.1, 
                                  ratioP > 0.9 & ratioP < 1.1, 
                                  SD > 1.0)
nrow(filtered_data)

#Or do this with a function
my_criteria <- function(ED, MAS, ratioE, ratioP, SD){
  MAS < 0.4 & MAS > 0.05 &
    ED > 34 &
    ratioE > 0.9 & ratioE < 1.1 & 
    ratioP > 0.9 & ratioP < 1.1 &
    SD > 1.0
}

filtered_data2 <- data2 %>% filter(my_criteria(ED, MAS, ratioE, ratioP, SD) == TRUE)
nrow(filtered_data2)

#Criteria used for the minimum requirements (my_criteriaMin):
    #Mean Seroprevalence between 5-40%
    #Persistence for at least 34 years
    #At least 1 spike detected

my_criteriaMin <- function(ED, MAS, SD){
  MAS < 0.4 & MAS > 0.05 &
    ED > 34 &
    SD > 1.0
}

filtered_dataMin <- data2 %>% filter(my_criteriaMin(ED, MAS, SD) == TRUE)
nrow(filtered_dataMin)
################################################################################
#Visually assess the six scenarios

#Function to insert code into the sourced file
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

#Make an empty list
final.pops.list <- list()

#Run through the 6 scenarios
for(i in 1:nrow(filtered_data2)){
  
  #Source through line 91 - after loading the parameters
  source2(file = "./All_Files_For_Publication/1- Run RVFV Simulations.R", start = 1, end = 155) #load param_vec
  #Change selected params
  param_vec["muAE"] <- filtered_data2[i,"muAE"]
  param_vec["NAEmax"] <- filtered_data2[i,"NAEmax"]
  param_vec["biteA"] <- filtered_data2[i,"biteA"]
  param_vec["NCEmax"] <- filtered_data2[i,"NCEmax"]
  param_vec["biteC"] <- filtered_data2[i,"biteC"]
  param_vec["q"] <- filtered_data2[i,"q"]
  #Run model
  source2(file = "./All_Files_For_Publication/1- Run RVFV Simulations.R", start = 156, end = 248)
  #Save to list
  final.pops.list[[i]] <- final.populations
  
  SheepScen_plot <- SLplot  + plot_theme +
    theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
          legend.key.size = unit(.15, "cm"))
  MosqScen_plot <- MosqIAll + plot_theme +
    theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
          legend.key.size = unit(.15, "cm"))
  
  #Name figure
  assign(sprintf("Sh_Scenario_%d", i), SheepScen_plot)
  assign(sprintf("M_Scenario_%d", i), MosqScen_plot)
  print(SLplot+labs(title = i))
  print(MosqIAll+labs(title = i))
  print(i)
}


# Select simulation 2 based on visual assessment added that to Model 1 - RVFV Selected Parameters.R

#include 1 and 6 as examples of simulations that were not included in supplemental figures for the methods supplement
fil.name <- "Publication_Figures/Fig S13 Examples of parameters that were not selected for the final model.pdf"
FigS13 <- ggarrange(Sh_Scenario_2, Sh_Scenario_1, Sh_Scenario_6, draw = FALSE,
                  labels = c("A", "B", "C"),
                  ncol = 1, nrow = 3)

ggexport(FigS13[1], filename = fil.name, width=5, height=5)




