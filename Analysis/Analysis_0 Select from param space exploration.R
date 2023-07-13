library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(egg)
library(ggpubr)
library(grid)
library(png)


#################
# To obtain these datafiles, I ran a version of your sensivity analysis - see file - "Run_this.R"
# that only varied the 6 parameters that we need to estimate. 
# This required adjustment of "Model1 - RVFV Optimized parameters" so that only the 6 are being varied.
# I also added some additional functions to "Function 1 Define functions... " and made the necessary corresponding
# adjustments to "Function 6 Latin Hypercube sensitvity analysis"

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
  source2(file = "./All_Files_For_Publication/1- Run RVFV Simulations.R", start = 1, end = 92) #load param_vec
  #Change selected params
  param_vec["muAE"] <- filtered_data2[i,"muAE"]
  param_vec["NAEmax"] <- filtered_data2[i,"NAEmax"]
  param_vec["biteA"] <- filtered_data2[i,"biteA"]
  param_vec["NCEmax"] <- filtered_data2[i,"NCEmax"]
  param_vec["biteC"] <- filtered_data2[i,"biteC"]
  param_vec["q"] <- filtered_data2[i,"q"]
  #Run model
  source2(file = "./All_Files_For_Publication/1- Run RVFV Simulations.R", start = 93, end = 190)
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
fil.name <- "Publication_Figures/Fig S12 Examples of parameters that were not selected for the final model.pdf"
FigS12 <- ggarrange(Sh_Scenario_2, Sh_Scenario_1, Sh_Scenario_6, draw = FALSE,
                  labels = c("A", "B", "C"),
                  ncol = 1, nrow = 3)

ggexport(FigS12[1], filename = fil.name, width=5, height=5)
#ggexport(FigS12[1], filename = fil.name, width=593, height=750, ncol = 1,nrow = 3)




############
#This code is defunct because we switched to Noam's plot to demonstrate this
##1) Extinction day
#plot_ED <- data2 %>% ggplot() +
#  geom_histogram(aes(x = ED), fill = "red", breaks = seq(0,35,1)) +
#  xlab("Extinction Day") + coord_cartesian(xlim = c(0, 35)) +
#  #gghighlight(ED > 34)
#  gghighlight(my_criteria2(ED, MAS,  outbreak) == TRUE) #ratioE, ratioP,
#plot_ED
#
#######################################
##2) MAS
#plot_MAS <- data2 %>% ggplot() +
#  geom_histogram(aes(x = MAS), fill = "red") +
#  xlab("Mean annual seroprevalence") +
#  ylim(0,10000) +
#  gghighlight(my_criteria2(ED, MAS, outbreak) == TRUE) #ratioE, ratioP, 
#
#plot_MAS
#
#######################################
##3) outbreak
#plot_outbreak <- data2 %>% ggplot() +
#  geom_histogram(aes(x = outbreak), fill = "red")+#, breaks = seq(0,10,1)) +
#  xlab("Outbreak measure") + #coord_cartesian(xlim = c(0, 10)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_outbreak
#
#p<- 1
#first <- TRUE
#data2$outbr_gap <- NA
#
#check <- c(seq(1,10, 1), seq(50,250,50))
#for(br in check){
#  if(first ==TRUE){
#  for(i in 1:nrow(data2)){
#        if(data2$outbreak[i] <= br){
#        data2$outbr_gap[i] <- p
#        p <- p +1
#        }
#  }
#      first <- FALSE
#      }else{
#        for(i in 1:nrow(data2)){
#          ind <- which(br==check)-1
#        if(data2$outbreak[i] <= br & data2$outbreak[i] >= check[ind]){#If the outbreak measure is between the break and the previous break, it gets counted in this bin
#          data2$outbr_gap[i] <- p
#          p <- p +1
#      }
#      }
#  }
#  p <- 1
#}
#
#
#data2$myFacet <- data2$outbr_gap < 5000
##
##bin_sums <- as.vector(table(cut(data2$outbreak, breaks=seq(0,10, by=1))))
##  
##df2 <- as.data.frame(matrix(ncol = 2, nrow = 10))
##
##names(df2) <- c("Outbreak_measure", "Count")
##
##measures <- as.vector(seq(0.5, 9.5, 1))
##df2$Count <- bin_sums
##df2$Outbreak_measure <- measures
#
#plot_outbreak <- data2 %>% ggplot() +
#  geom_histogram(aes(x = outbreak), fill = "red", breaks = seq(0,10,1)) +
#  xlab("Outbreak measure") + coord_cartesian(xlim = c(0, 10)) + #Missing our target which has higher values
#  gghighlight(my_criteria2(ED, MAS, outbreak) == TRUE) #ratioE, ratioP, 
#plot_outbreak
#
##With Break
#plot_outbreak <- data2 %>% ggplot() +
#  geom_point(aes(x = outbreak, y = outbr_gap), color = "red") +
#  labs(x="Outbreak measure", y = "Number of simulations") + 
#  facet_grid(rows = vars(myFacet), scales="free") +#, space="free") +
#  scale_x_continuous(breaks = seq(0, 200, 30)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) +
#  theme(strip.text.y = element_blank()) 
#
##No break
#plot_outbreak <- data2 %>% ggplot() +
#  geom_point(aes(x = outbreak, y = outbr_gap), color = "red") +
#  labs(x="Outbreak measure", y = "Number of simulations") + 
#  #facet_grid(rows = vars(myFacet), scales="free") +#, space="free") +
#  #scale_x_continuous(breaks = seq(0, 200, 30)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) #+
#  #theme(strip.text.y = element_blank()) 
#
##(ggplot(df, aes(y=myLetter, x=myValue)) 
##  + geom_point() 
##  + facet_grid(r, scales="free", space="free")+
##  + scale_x_continuous(breaks = seq(0, 5, .25)) # this gives both facets equal interval spacing.
##  + theme(strip.text.x = element_blank()) # get rid of the facet labels
##)
#
#######################################
##4) ratioE
#plot_ratioE <- data2 %>% ggplot() +
#  geom_histogram(aes(x = ratioE), fill = "red") +
#  xlab("Ratio eggs") + coord_cartesian(xlim = c(0, 5)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_ratioE
#
#param.list <- c("muAE", "biteA", "biteC", "q", "ratioP", "ratioE",  "ED", "outbreak", "MAS", 
#                "NAEmax", "NCEmax")
#
#
#
#Plot.Param.Space <- function(df, param.list){
#  #small.param <- c("muAE", "biteA", "biteC", "q") 
#  #med.param <- c("ratioP", "ratioE",  "ED")
#  #lg.param <- c("outbreak", "MAS")
#  # huge.param <- c("NAEmax", "NCEmax")
#  for(param in param.list){
#      #Determine bin sizes
#  param.range <- c(param.range, range(df[param]))}
#  max.param <- max(param.range)
#  min.param <- min(param.range)
#   bin.sizes <- c(seq(min.param*1.1,max.param*1.1, max.param/10))
#   #Set the bins mannually for each parameter that we might plot as some have a lot of data around 1, and some data 100X or more
#   bin.name <- paste(param, "bins", sep = "_")
#   
#  p<- 1
#  first <- TRUE
#  df$bin_cols <- NA
#  
#  #Sort into bins - within each bin each data point will be numbered consecutively. Thus when we plot with geom_point, it will look like a histogram
#for(bz in bin.sizes){#For each bin
#  if(first ==TRUE){#the first time it goes through (e.g. br == 1), this will mark all data rowns that belong in the first bin and leave the rest at NAs
#      for(i in 1:nrow(df)){
#
#        if(df[i,param] <= bz){#If the parameter value is less than the first bin
#          df$bin_cols[i] <- p #add it to the count for that bin
#        p <- p +1
#      }
#    }
#  first <- FALSE
#  }else{
#  for(i in 1:nrow(df)){#After the first time
#      ind <- which(bz==bin.sizes)-1#Set index of which was the value of the previous bin
#      if(df[i,param] <= bz & df[i,param] >= bin.sizes[ind]){ #If it's between the max of our current bin and the max of the previous bin then it should be counted for this bin
#        df$bin_cols[i] <- p
#        p <- p +1
#      }
#    }
#  }
#  p <- 1
#}
#}
#  
#  names(df$bin_col) <- bin.name
#
#plot_ratioE <- data2 %>% ggplot() +
#  geom_point(aes(x = ratioE, y = ratE_gap), color = "red") +
#  labs(x="Ratio eggs", y = "Number of simulations") + 
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) #+
#plot_ratioE
##
#######################################
##5) ratioP
#plot_ratioP <- data2 %>% ggplot() +
#  geom_histogram(aes(x = ratioP), fill = "red") +
#  xlab("Ratio seroprevalence") + coord_cartesian(xlim = c(0, 5)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_ratioP
#
#################################################################################
#max_y <- 100
#######################################
##6) q
#mean_q <- signif(mean(filtered_data2$q),2)
#plot_q <- data2 %>% ggplot() +
#  geom_histogram(aes(x = q), fill = "red", breaks = seq(0,1,0.1)) +
#  xlab("q") + coord_cartesian(xlim = c(0, 1)) + ggtitle(paste0("Mean q = ", mean_q)) +
#  coord_cartesian(ylim = c(0,max_y)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_q
#
#######################################
##6) biteA
#mean_biteA <- signif(mean(filtered_data2$biteA),2)
#plot_biteA <- data2 %>% ggplot() +
#  geom_histogram(aes(x = biteA), fill = "red") +
#  xlab("biteA") + ggtitle(paste0("Mean biteA = ", mean_biteA)) +
#  coord_cartesian(ylim = c(0,max_y)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_biteA
#
#######################################
##7) biteC
#mean_biteC <- signif(mean(filtered_data2$biteC),2)
#plot_biteC <- data2 %>% ggplot() +
#  geom_histogram(aes(x = biteC), fill = "red") +
#  xlab("biteC") + ggtitle(paste0("Mean biteC = ", mean_biteC)) +
#  coord_cartesian(ylim = c(0,max_y)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_biteC
#
#######################################
##7) muAE
#mean_muAE <- signif(mean(filtered_data2$muAE),2)
#plot_muAE <- data2 %>% ggplot() +
#  geom_histogram(aes(x = muAE), fill = "red") +
#  xlab("biteC") + ggtitle(paste0("Mean muAE = ", mean_muAE)) +
#  coord_cartesian(ylim = c(0,max_y)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_muAE
#
#######################################
##8) NAEmax
#mean_NAEmax <- signif(mean(filtered_data2$NAEmax),3)
#plot_NAEmax <- data2 %>% ggplot() +
#  geom_histogram(aes(x = NAEmax), fill = "red") +
#  xlab("NAE max") + ggtitle(paste0("Mean NAEmax = ", mean_NAEmax)) +
#  coord_cartesian(ylim = c(0,max_y)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE) 
#plot_NAEmax
#
#######################################
##9) NAEmax
#mean_NCEmax <- signif(mean(filtered_data2$NCEmax),3)
#plot_NCEmax <- data2 %>% ggplot() +
#  geom_histogram(aes(x = NCEmax), fill = "red") +
#  xlab("NAE max") + ggtitle(paste0("Mean NCEmax = ", mean_NCEmax)) +
#  coord_cartesian(ylim = c(0,max_y)) +
#  gghighlight(my_criteria(ED, MAS, ratioE, ratioP, outbreak) == TRUE)
#plot_NCEmax
#######################################
#
#