#' ---
#' title: "RVFV SIRS Model Sheep Aedes and Culex"
#' author: "Mindy Rostal"
#' date: "March 28, 2022"
#' output: html_document
#' ---
#' 
#' This simulates the transmission of Rift Valley fever virus (RVFV) to sheep from *Aedes* and *Culex* mosquitoes.
#' 
#' Here S = number of susceptibles, I = number of infecteds, E = number exposed, V = number vaccinated, A = number with maternal immunity and R = number of recovereds.

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

#Set Scenarios  - User decides at the start
Vaccinate <- FALSE #Change to true is you want to run a simulation where you vaccinate the sheep and select a vaccination %
  vax.proportion <- .26#Proportion of the flock user wants to simulate vaccination in
  vax.25_higher <- FALSE #Run simulation with a host vaccination rate that is 25% higher
  vax.25_lower <- FALSE #Run simulation with a host vaccination rate that is 25% higher

No.q <- FALSE #No transovarial transmission
Only.q <- FALSE #No horizontal transmission
muC.25_higher <- FALSE #Run simulation with a Culex mortality rate that is 25% higher
muC.25_lower <- FALSE #Run simulation with a Culex mortality rate that is 25% lower

#Always False:
SA <- FALSE #This should always be FALSE: If you want to run a sensitivity analysis, you need to use a different simulation file 

set.seed(6242015)#This is to ensure the approxfun function always outputs the same mosquito hatching patterns

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


#Set parameters for various scenarios if they are selected
if(Vaccinate == TRUE){
  #Calculation: p = vax/(vax+muL+g); p(vax +muL + g) = vax;  pvax + p(muL + g) = vax; p(muL + g) = (1-p)vax; vax = p*(muL+g)/(1-p) 
  if(vax.25_higher == FALSE & vax.25_lower == FALSE){ 
  param_vec["vax.prop"] <- vax.proportion
  }
  
  if(vax.25_higher == TRUE){
    param_vec["vax.prop"] <- vax.proportion * 1.250
  }
  
  if(vax.25_lower == TRUE){
    param_vec["vax.prop"] <- vax.proportion * 0.750
  }
  vax <- param_vec[["vax.prop"]]*(param_vec[["muL"]] + param_vec[["g"]])/ (1-param_vec[["vax.prop"]]) #vax = p*(muL+g)/(1-p)
  param_vec["vax"] <- vax
}

#No transovarial transmission
if(No.q == TRUE){
  param_vec["q"] <- 0
}

#No horizontal transmission
if(Only.q == TRUE){
  param_vec["Tcsl"] <- 0
  param_vec["Tslc"] <- 0
  param_vec["Tasl"] <- 0
  }

#Culex mortality rate
if(muC.25_higher == TRUE){
  param_vec["muC"] <- param_vec[["muC"]] * 1.25
}

if(muC.25_lower == TRUE){
  param_vec["muC"] <- param_vec[["muC"]] * 0.75
}

#Set time
start.time <- 0
end.time <- tail(All_Precip$SimDay, 1) #3000#
timestep <- 1
times <- seq(from=start.time, to=end.time, by=timestep)

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


###Plots to check results quickly, to plot all together use the multiplot analysis file
#Set ggplot theme
thesis_theme <- theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.title = element_text(size = rel(1.5))) + 
  theme(legend.text = element_text(size = 10)) + 
  theme(plot.title = element_text(size=16))

SLplot <-ggplot(final.populations, aes(x=time)) + 
  geom_line(aes(y = VS, colour = "VS")) +#Put VS and VL first so that if it is zero the other lines are on top
  geom_line(aes(y = VL, colour = "VL")) +
  geom_line(aes(y = SS, colour = "SS")) + 
  geom_line(aes(y = RS, colour = "RS")) +
  geom_line(aes(y = SL, colour = "SL")) + 
  geom_line(aes(y = RL, colour = "RL")) +
  geom_line(aes(y = AL, colour = "AL")) +
  geom_line(aes(y = IS, colour = "IS")) +
  geom_line(aes(y = IL, colour = "IL")) +
  geom_vline(xintercept = outbreak2010, color = "dark blue")+#This is 2-15-2010
  labs( x = "Year", y = "Sheep and \nLambs")+
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 1, to = end, by = 365)) +
  scale_colour_manual("", values =c("SS" = "black","IS"= "red", "RS" = "green", "VS" = "blue", "SL" = "gray","IL"= "pink", "RL" = "light green", "VL" = "light blue", "AL" = "violet")) +
  thesis_theme+
  theme(plot.margin=unit(c(1,.5,.5,.5),"cm"),#top, right, bottom and left.
        legend.key.size = unit(.25, "cm"))#decrease space between the legend items


SLplot


MosqIAEplot <- ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = IAE, colour = "IAE")) + 
  labs(x = "Time", y = "Infected \nAedes Eggs") +
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end, by = 365)) +
  scale_colour_manual("", values =c("IAE" = "red")) +
  thesis_theme + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))

MosqIAEplot

MosqIAll <-ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = IA, colour = "IA")) + 
  geom_line(aes(y = IC, colour = "IC")) +
  geom_vline(xintercept = outbreak2010, color = "dark blue")+#This is 3-1-2010
  labs( x = "Time", y = "Infected \nMosquitoes")+ 
  #coord_fixed(ratio = 5)+
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end, by = 365)) +
  scale_colour_manual("", values =c("IA" = "red","IC"= "pink")) +
  thesis_theme + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))

MosqIAll

#Get outbreak dataframe
Outbreaks <- Get.Outbreak.Dataframe(final.populations, param_vec$sigma_sl)

#Get peak infected populations
PeakInfectedMosq <- InfectedMosqPeak(final.populations)

PeakInfectedMosq <- full_join(PeakInfectedMosq, Outbreaks)

#remove 2017 since only 1/2 a MosqYear
PeakInfectedMosq <- filter(PeakInfectedMosq, !MosqYear == 2017)

PeakIMosq_Plot <- ggplot(PeakInfectedMosq, aes(x = Ratio_iCu_to_iAe, y = Endemic))+
  geom_point()+ 
  labs( x = "Ratio of Infected Culex to Infected Aedes", y = "Infected Animals")+ #"Difference between Infected Culex and Aedes Population Sizes"
  thesis_theme + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))

#Number of surviving Culex mosquitoes in August of each year (end of season)
minCpop_MosqYR <- final.populations%>% 
  filter(Month == 8) %>% 
  group_by(MosqYear)%>%
  summarise(MinSC = min(SC), MinEC = min(EC), MinIC = min(IC))%>% 
  mutate(across(.fns = ~replace(., . <1 , 0)))

#Number of surviving Aedes mosquitoes
minApop_MosqYR <- final.populations%>% 
  filter(Month == 8) %>% 
  group_by(MosqYear)%>%
  summarise(MinSA = min(SA), MinEA = min(EA), MinIA = min(IA))%>% 
  mutate(across(.fns = ~replace(., . <1 , 0)))

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

  #Calculate the mean population sizes
final.populations <- final.populations%>%
  mutate(NSL = NS + NL)%>%
  mutate(All_AEggs = SAE + IAE)

m_NS <- mean(final.populations$NS, na.rm = TRUE)
m_NL <- mean(final.populations$NL, na.rm = TRUE)
m_NSL <- mean(final.populations$NSL, na.rm = TRUE)
m_AE <- mean(final.populations$All_AEggs, na.rm = TRUE)

#Mean population sizes across ALL years when a mosquito of the relevant species is present
#Aedes
m_NAedes <- Get.Mean.Vec.Pops(final.populations, "NAedes")[1]
max_NAedes <- Get.Mean.Vec.Pops(final.populations, "NAedes")[2]
m_peak_NAedes <- Get.Mean.Vec.Pops(final.populations, "NAedes")[3]

#Culex
m_NC <- Get.Mean.Vec.Pops(final.populations, "NC")[1]
max_NC <- Get.Mean.Vec.Pops(final.populations, "NC")[2]
m_peak_NC <- Get.Mean.Vec.Pops(final.populations, "NC")[3]

#Table S6 
#The proportion of Aedes eggs that are infected
Mean_IAE_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IAE")[1],3)
Min_IAE_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IAE")[2],3)
Max_IAE_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IAE")[3],3)

#Calculate the ratio of infected mosquitoes to total pop for Aedes and Culex
Mean_IA_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IA")[1],3)
Min_IA_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IA")[2],3)
Max_IA_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IA")[3],3)

Mean_IC_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IC")[1],3)
Min_IC_Prop = Get.Prop.Infected.Mosq(final.populations, "IC")[2]
Max_IC_Prop = round(Get.Prop.Infected.Mosq(final.populations, "IC")[3],3)

#Calculate host-vector ratios
#Calculate ratios
HV_ratio <- final.populations%>%
  mutate(NA_SL_Ratio = (NS +NL)/NAedes)%>%
  mutate(NC_SL_Ratio = (NS + NL)/NC)%>%
  mutate(Mosq_SL_Ratio = (NS +NL)/(NAedes+NC))

#Remove infinity and calculate minimum ratios per year
HV_ratio <- HV_ratio%>%
  filter(!Mosq_SL_Ratio == "Inf")%>%
  filter(!Mosq_SL_Ratio == "-Inf")%>%
  group_by(MosqYear)%>%
  summarise(minNA_SL_Ratio = min(NA_SL_Ratio),
            minNC_SL_Ratio = min(NC_SL_Ratio),
            minMosq_SL_Ratio = min(Mosq_SL_Ratio))
  
mean_max_Host_Mosq_Ratio = round("^"(mean(HV_ratio$minMosq_SL_Ratio),-1),0)
min_max_Host_Mosq_Ratio = round("^"(min(HV_ratio$minMosq_SL_Ratio),-1),0)
max_max_Host_Mosq_Ratio = round("^"(max(HV_ratio$minMosq_SL_Ratio),-1),0)
      
#Quick check to sure the model is working (no negative populations)
pop_check <- select(final.populations,  -MosqDay, -MosqYear, -Month)
ifelse(any(pop_check< -.99), "ERROR!!!!", "Proceed")

#Outbreaks by year
#Get outbreak length  - number of days per year that the infected hosts are present
O.Days <- final.populations%>%
  filter(!IS <1 | !IL <1)%>%
  group_by(MosqYear)%>%
  summarise(Length = n())

Outbreaks <- full_join(Outbreaks, O.Days, by = "MosqYear")

Outbreaks <- Outbreaks%>%
  mutate(Length = replace(Length, is.na(Length) & Endemic == 0, 0))

#Outbreak size and incidence
out <- Outbreaks[2:35,]#Remove first and last year
meanI <- mean(out$Endemic)
meanLength <- mean(out$Length)

mean.inc.per.100 <- meanI/m_NSL*100
meanSeroP <- Get.Mean.SeroP.MosqY(final.populations)


End.Seroprevalence <- tail(final.populations$RS, 1)/(tail(final.populations$SS, 1) + tail(final.populations$RS, 1)+tail(final.populations$IS, 1))

#Mean seroprevalence per year inclucding vaccinated sheep
Seroprev <- final.populations%>%
  mutate(RL = replace(RL, RL < 1, 0))%>%
  mutate(RS = replace(RS, RS < 1, 0))%>%
  mutate(NS = replace(NS, NS < 1, 0))%>%
  mutate(NL = replace(NL, NL < 1, 0))%>%
  mutate(SeroP_SL = (RS + RL)/(NS+NL))%>%
  mutate(infected = (IS + IL)/(NS+NL))%>%
  mutate(vax_SL = (VS + VL)/(NS+NL))%>%
  mutate(SeroP_vax = (VS + RS + VL + RL)/(NS+NL))%>%
  group_by(MosqYear)%>%
  summarise(SeroP_SL = max(SeroP_SL),
            infect_SL = max(infected),
            Vax_SP = max(vax_SL),
            SeroP_vax = max(SeroP_vax))

median.serop <- round(median(Seroprev$SeroP_vax)*100,1)#Should be the same as above
mean.serop <- round(mean(Seroprev$SeroP_vax)*100,1)#Should be the same as above
range.serop <- round(range(Seroprev$SeroP_vax)*100,1)

#Make Table S6
Ratio_Tab <- data.frame(matrix(ncol = 2, nrow = 5))

Ratio_Tab[1,] <- c("Mean annual seroprevalence", paste0( mean.serop, "% (", range.serop[1], "-",  range.serop[2], "%)"))
Ratio_Tab[2,] <- c("Mean annual maximum host-vector ratio", paste0("1:", mean_max_Host_Mosq_Ratio, " (1:", max_max_Host_Mosq_Ratio, "-1:", min_max_Host_Mosq_Ratio, ")"))
Ratio_Tab[3,] <- c("Mean annual proportion of infected Aedes eggs", paste0( Mean_IAE_Prop, " (", Min_IAE_Prop, "-",  Max_IAE_Prop, ")"))
Ratio_Tab[4,] <- c("Mean annual proportion of infected adult Aedes", paste0( mean.IA_NAedesRatio, " (", round(range.IA_NAedesRatio[1],4), "-", round(range.IA_NAedesRatio[2],2), ")"))
Ratio_Tab[5,] <- c("Mean annual proportion of infected adult Culex", paste0( mean.IC_NCRatio, " (", round(range.IC_NCRatio[1],7), "-",  round(range.IC_NCRatio[2],4), ")"))

nmes.rat.tab <- c("Factor", "mean (range)")

names(Ratio_Tab) <- nmes.rat.tab

if(muC.25_lower == FALSE & muC.25_higher == FALSE & vax.25_lower == FALSE & vax.25_higher == FALSE){
  if(No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE){
write.csv(Ratio_Tab, "./Publication_Figures/Table 1 infected proportions host-vector ratios and seroprevalence_publication.csv", row.names = FALSE)
  }
}

#calculate vax.prop from vax
vxp <- (unlist(param_vec["vax"])/(unlist(param_vec["muL"])+ unlist(param_vec["g"]) + unlist(param_vec["vax"])))


  ###################################
  #Add Effective R0 to final.populations
  R_pops <- select(final.populations, SS, SL, SA, SC, MosqYear)
  
  Reff.list <- apply(R_pops, 1, function(x) R_eff(x,  params = R0params, get.fun1 = calc_R0))

  final.populations$Reff <- Reff.list  

  print(paste("The ending seroprevalence is ", round(End.Seroprevalence, 2), sep = ""))
  print(paste0("The mean incidence rate per 100 sheep-years is ", round(mean.inc.per.100,1), "."))
    
  