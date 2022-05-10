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

#Set parameters
Check_Q <- TRUE
Check_Tasl <- FALSE
Check_Tsla <- FALSE
Check_epsilon <- FALSE

#Always False:
SA <- FALSE #Sensitivity analysis, needs a different simulation file 
Vaccinate <- FALSE #Change to true is you want to run a simulation where you vaccinate the sheep and select a vaccination %
No.q <- FALSE #No transovarial transmission
Only.q <- FALSE #No horizontal transmission

set.seed(6242015)#This is to ensure the approxfun function always outputs the same mosquito hatching patterns

#Source other code
#Source assessment functions
source(here("Functions", "Function 1 Define Functions for Output of Sensitivity Analysis.R"))
#Load ODE function
source(here("Model_Scripts", "Model 3 RVFV ODE SIRS function.R"))
#Load plotting functions
source(here("Functions", "Function 3 Plot simulations of a given time period.R"))
#Source climate dataframe - this sources "RVFV Optimized Parameters for publication.R" 
source(here("Model_Scripts", "Model 2 Mosquito hatch rates at daily timestep.R"))

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

#Create vector of values to simulate, including the value for that parameter that is used in the model
vec <- seq(0,1, 0.05) 

vec <- c(vec, param_vec[param])

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

#Add column names
names(tab) <- c("Mean_Seroprevalence", "Extinction_Day", "Parameter_Value", "Parameter_Tested")

#Create file names
data.file.title <- paste("Data for sensitivity analyses/", param, " vs Mean Seroprevalence and Extinction Day.Rdata", sep = "")
csv.file.title <- paste("Data for sensitivity analyses/", param, " vs Mean Seroprevalence and Extinction Day.csv", sep = "")

#Save data files
save(tab, file = data.file.title )#Save the data
write.csv(tab, csv.file.title, row.names = FALSE)


#Make plots for a single parameter vs mean seroprevalence and persistence
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

fil.name <- paste0("Publication_Figures/Draft_Figures/Fig of ", param, " vs mean seroprevalence and persistence.png")
ggexport(Fig, filename = fil.name, width=522, height=305, ncol = 2,nrow = 1)