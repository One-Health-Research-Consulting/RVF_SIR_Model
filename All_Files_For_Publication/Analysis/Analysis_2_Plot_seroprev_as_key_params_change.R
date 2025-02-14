#' ---
#' title: "RVFV SEIR Model examining parameter by parameter relationships"
#' author: "Mindy Rostal"
#' date: "April 28, 2022"
#' output: html_document
#' ---
#' 
#' This simulates the effect on persistence and seroprevalence and R0 when the value of select parameter pairs are varied.  
#' 
#' Here S = number of susceptibles, I = number of infecteds and R = number of recovereds.

#Load packages:
library(codetools)
library(deSolve)
library(zoo)
library(dplyr)
library(here)
library(lhs)
library(RColorBrewer)
library(egg)
library(ggplot2)
library(ggpubr)
library(ggtext)

#Set variables do you want to compare and using which output
Q_biteA_Seroprev.mean <- TRUE #Transovarial Transmission vs Aedes bite rate evaluating seroprevalence
  R0_plot <- TRUE              #Also examine the effect on R0 and both persistence and seroprevalence - works with Q_biteA_Seroprev.mean == TRUE
Q_muC_Seroprev.mean <- FALSE  #Transovarial Transmission vs Culex mortality rate evaluating seroprevalence; Not for plot, but for single line in results
Vax_Q_Persistence <- FALSE    #Transovarial Transmission vs vaccination rate evaluating persistence
Q_Tasl_Persistence <- FALSE   #Transovarial Transmission vs host-to-Aedes transmission rate evaluating persistence
Q_IAE_Persistence <- FALSE     # Transovarial Transmission vs the initial infected egg population

#No scenarios are evaluated by this code, these should always be FALSE:
SA <- FALSE #This should always be FALSE: If you want to run a sensitivity analysis, you need to use a different simulation file 
Var_Select <- FALSE #This should always be FALSE: If you want to run a sensitivity analysis, you need to use a different simulation file 
Vaccinate <- FALSE #Change to true if you want to run a simulation where you vaccinate the lambs and select a vaccination %
No.q <- FALSE #No transovarial transmission
Only.q <- FALSE #No horizontal transmission

if(Vax_Q_Persistence == TRUE){
  Vaccinate <- TRUE #Need this true to get sigvax to do the daily vaccination rate - otherwise it will be 0 even if you put in a vax.prop value
}

set.seed(6242015)#This is to ensure the approxfun function always outputs the same mosquito hatching patterns

#Source code
#Source  functions
source(here("All_Files_For_Publication/Functions", "Function 1 Define Functions for Output of Sensitivity Analysis.R"))
#Source model definitions
source(here("All_Files_For_Publication/Model_Scripts", "Model 2 Mosquito hatch rates at daily timestep.R"))
#Source ODE function
source(here("All_Files_For_Publication/Model_Scripts", "Model 3 RVFV ODE SIRS function.R"))
#Source Function to calculate R0
source(here("All_Files_For_Publication/Functions", "Function 4 Calculate R0.R"))

#Set up each analysis
#Set scales - see how seroprevelance changes with different combinations of q and bite rates
q_vec_R01 <- c(0, .1, .2, .3, .4, .5, .6, param_vec$q, .8, .9, .99)#q vector
q_vec <- c(0, 0.25, 0.5, param_vec$q, 0.75, 0.99)#q vector
biteA_vec <- seq(from = .05, to = 1, by = .025) #biteA vector
biteA_vec_R01 <- seq(from = .01, to = 0.45, by = .01)#Smaller range and increments to examine R0 around 1
muC_vec_R01 <- c( 0.0565003, seq(from = .01, to = 0.33, by = .025))#With a death rate >~0.35 Culex populations are so exponentially small, it breaks the model
Tasl_vec <- seq(from = 0.05, to = 1, by = 0.05)#host-to-Aedes transmission 
IAE_vec <- c(seq(0.0001, 0.005, by = 0.0005), 0.0008, 0.001) #ratio of infected to non-infected eggs
#Vaccination
Prop_vax_vec <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, .99)#proportion of hosts vaccinated

#Set names of all parameters
var_q <- "q"
var_biteA <- "biteA"
var_muC <- "muC"
var_vax <- "vax"
var_Tasl <- "Tasl"
# for variables
var_IAE <- "IAE"
#for vaccination calculations
var_g <- "g"
var_muL <- "muL"

#Outcome measures
outcome_extinct <- "Day_of_Extinction"
outcome_serop_mean <- "Mean_Annual_Seroprevalence"

#Set all outcome functions
fun_serop_mean <- Get.Mean.SeroP.MosqY
fun_ext <- Get.Extinct.Day

#Set variables for each parameter pair
if(Q_biteA_Seroprev.mean == TRUE){
  discrete_vec <- q_vec
  near.continuous_vec <- biteA_vec
  if(R0_plot == TRUE){
    near.continuous_vec <- biteA_vec_R01
    fun.2 <- fun_ext
    discrete_vec <- q_vec_R01
  }
  var1 <- var_q
  var2 <- var_biteA
  outcome <- outcome_serop_mean
  fun.1 <- fun_serop_mean
  var1.plot <- "Transovarial Transmission"
  var2.plot <- "Aedes Bite Rate"
  outcome.plot <- "Mean Annual Seroprevalence"
}

if(Q_muC_Seroprev.mean == TRUE){
  discrete_vec <- q_vec
  near.continuous_vec <- muC_vec_R01
  if(R0_plot == TRUE){
    near.continuous_vec <- muC_vec_R01
    fun.2 <- fun_ext
    discrete_vec <- q_vec_R01
  }
  var1 <- var_q
  var2 <- var_muC
  outcome <- outcome_serop_mean
  fun.1 <- fun_serop_mean
  var1.plot <- "Transovarial Transmission"
  var2.plot <- "Culex mortality Rate"
  outcome.plot <- "Mean Annual Seroprevalence"
}

if(Vax_Q_Persistence == TRUE){ 
  discrete_vec <- q_vec
  var1 <- var_q
  var2 <- var_vax
  var3 <- var_g
  var5 <- var_muL
  vax_vec <- Prop_vax_vec*(as.numeric(param_vec[var5]) + as.numeric(param_vec[var3]))/ (1-Prop_vax_vec)
  near.continuous_vec <- vax_vec
  outcome <- outcome_extinct
  fun.1 <- fun_ext
  var1.plot <- "Transovarial Transmission"
  var2.plot <- "Percent of Flock Vaccinated"
  outcome.plot <- "Day of Extinction"
  
  #If we are looking at vaccine levels we need to hold the original starting values for R and S sheep and lambs to change it back at the end of the the vax loop
  origRS <- initial.populations[["RS"]]
  origSS <- initial.populations[["SS"]]
  origVS <- initial.populations[["VS"]]
  origRL <- initial.populations[["RL"]]
  origSL <- initial.populations[["SL"]]
  origVL <- initial.populations[["VL"]]
  origAL <- initial.populations[["AL"]]
}

if(Q_Tasl_Persistence == TRUE){
  discrete_vec <- q_vec
  near.continuous_vec <- Tasl_vec
  var1 <- var_q
  var2 <- var_Tasl
  outcome <- outcome_extinct
  fun.1 <- fun_ext
  var1.plot <- "Transovarial Transmission"
  var2.plot <- "Transmission from Host to Aedes"
  outcome.plot <- "Day of Extinction"
}

if(Q_IAE_Persistence == TRUE){
  discrete_vec <- q_vec
  near.continuous_vec <- IAE_vec
  var1 <- var_q
  var2 <- var_IAE
  outcome <- outcome_extinct
  fun.1 <- fun_ext
  var1.plot <- "Transovarial Transmission"
  var2.plot <- "Ratio of Susceptible to Infected Aedes Eggs"
  outcome.plot <- "Day of Extinction"
}

#Make blank dataframe
r <- length(discrete_vec)
z <- length(near.continuous_vec)

num.rows <- r*z
summary.dat <- data.frame(matrix(nrow = num.rows, ncol = 4))
names(summary.dat) <- c(var1, var2, outcome, "mean_popn_R0")

j <- 1    

set.seed(6242015)

#Set time
start.time <- 0
end.time <- tail(All_Precip$SimDay, 1) #3000#
timestep <- 1
times <- seq(from=start.time, to=end.time, by=timestep)

#For each discrete var we want to run through each near.continuous var
for(param1.2 in discrete_vec){
  param_vec[var1] <- param1.2
  for(param2 in near.continuous_vec){
    if(Q_IAE_Persistence == FALSE){
    param_vec[var2] <- param2
    param1 <- param1.2
    
    #if needed set specific initial populations
    if(Vax_Q_Persistence  == TRUE){
      param2_inv <- round((param2/(as.numeric(param_vec[var5]) + as.numeric(param_vec[var3]) + param2)),2)
      NS = origRS + origSS + origVS
      initial.populations[["VS"]] <- (NS)*param2_inv
      initial.populations[["RS"]] <- origRS
      initial.populations[["SS"]] <- NS - initial.populations[["RS"]] - initial.populations[["VS"]]
      NL = origRL + origSL + origVL + origAL
      initial.populations[["VL"]] <- (NL)*param2_inv
      initial.populations[["RL"]] <- origRL
      initial.populations[["AL"]] <- origAL
      initial.populations[["SL"]] <- NL - initial.populations[["RL"]] - initial.populations[["VL"]] - initial.populations[["AL"]] 
    }
    }else{
      initial.populations[["IAE"]] <- initial.populations[["SAE"]]*param2
      param1 <- param1.2
    }
    
    set.seed(6242015)#Set seed so reproducible with approxfun    
    
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
    
    final.populations <- as.data.frame(matrix.populations)
    final.populations$time <- seq(1:length(final.populations$SS))
    #Add the year
    end <- length(times)-1
    Yr<-head(All_Precip, end)%>%
      select(Year)
    Yr<- rbind("1982", Yr)
    Yr<-as.data.frame(Yr)
    final.populations <- cbind(Yr, final.populations)
    final.populations$Year<-as.numeric(final.populations$Year)
    
    final.populations <- Define.MosqYr(final.populations, All_Precip)
    
    out <- fun.1(final.populations)
    
    if(R0_plot == TRUE){
      #Calculate mean populations sizes
      #Host
      m_NS <- mean(final.populations$NS, na.rm = TRUE)
      m_NL <- mean(final.populations$NL, na.rm = TRUE)
      
      #Mean population sizes across ALL years when a mosquito of the relevant species is present
      m_NAedes <- Get.Mean.Vec.Pops(final.populations, "NAedes")[1]
      m_NC <- Get.Mean.Vec.Pops(final.populations, "NC")[1]
      
      #Put populations into one vector
      mean_popn   <- c(NS = m_NS, NL = m_NL, Na = m_NAedes, NC = m_NC)
      
      #Calculate the mean dev_ALP, dev_CLP, mu_CLP and mu_ALP only from days when these compartments are non-zero
      ALP_rates <- final.populations%>%
        filter(SALP >=1 | IALP >=1)
      CLP_rates <- final.populations%>%
        filter(SCLP >=1)
      
      mdev_ALP <- mean(ALP_rates$dev_ALP) 
      mmu_ALP <- mean(ALP_rates$muALP) 
      
      mdev_CLP <- mean(CLP_rates$dev_CLP) 
      mmu_CLP <- mean(CLP_rates$muCLP) 
      
      #Calculate R0
      R0_est <- calc_R0(param_vec, mean_popn)
      
      #Calculate Persistence
      out.2 <- fun.2(final.populations)
    }
    
    if(Q_IAE_Persistence == FALSE){
    summary.dat[j, "param1"] <- param1
    summary.dat[j, var1] <- param_vec[var1]
    summary.dat[j, var2] <- param_vec[var2]
    summary.dat[j, outcome] <- out
    }else{
      summary.dat[j, "param1"] <- param1
      summary.dat[j, var1] <- param_vec[var1]
      summary.dat[j, var2] <- initial.populations[["IAE"]]/initial.populations[["SAE"]]
      summary.dat[j, outcome] <- out
    }
    
    if(R0_plot == TRUE){
      summary.dat[j, "mean_popn_R0"] <- R0_est
      summary.dat[j, "Persistence"] <- out.2
    }
    
    if(Vax_Q_Persistence  == TRUE){
      summary.dat[j, "PropVax"] <- param2_inv
    }
    
    if(Q_muC_Seroprev.mean == TRUE  & R0_plot == TRUE){
      ###################################
      #Add Effective R0 to final.populations
      R_pops <- select(final.populations, SS, SL, SA, SC, MosqYear)
      
      Reff.list <- apply(R_pops, 1, function(x) R_eff(x,  params = R0params, get.fun1 = calc_R0))
      
      final.populations$Reff <- Reff.list  
      
      Reff_pop <- final.populations%>%
        group_by(MosqYear)%>%
        summarize(Reff_mean = mean(Reff),
                  Reff_max = max(Reff))
      
      summary.dat[j, "mean_R0_Reff"] <- mean(Reff_pop$Reff_mean)
      summary.dat[j, "max_R0_Reff"] <- max(Reff_pop$Reff_max)
      summary.dat[j, "mean_max_R0_Reff"] <- mean(Reff_pop$Reff_max)
      
    }
    
    j <- j+1
    End.run.time <- Sys.time()
    print(End.run.time - start.run.time)
    print(j)
    
  }
}

#Add the value of R0 when q was 0 to dataframe as a new column
if(R0_plot == TRUE){
  if(any(summary.dat$q==0)){
    #Filter for q = 0
    R0_subset <- summary.dat %>% 
      filter(q == 0) 
    
    #Add columns
    R0_subset <- R0_subset[,c(var2, "mean_popn_R0")]
    
    #Join dataframes
    summary.dat <- full_join(summary.dat, R0_subset, by = var2) 
    #rename columns
    summary.dat <- summary.dat %>% 
      mutate(mean_popn_R0 = mean_popn_R0.x, R0_when_q0 = mean_popn_R0.y) %>%
      select(-mean_popn_R0.x, -mean_popn_R0.y)
    
    #Calculate seasonal and long-term persistence
    summary.dat <- summary.dat %>% 
      mutate(Persist_long = case_when(Persistence > 12165 ~ "yes", Persistence <= 12165 ~ "no")) %>%
      mutate(Persist_year = case_when(R0_when_q0 >= 1 ~ "yes", R0_when_q0 < 1 ~ "no"))%>%
      filter(!q== 0.75)#Too close to our value of 0.7, only need one.
  }
}

#Make var1 a factor so that ggplot won't try to color it on a gradient
summary.dat[var1] <- as.factor(as.character(summary.dat[, var1]))


#Titles
if( outcome == "Mean_Annual_Seroprevalence" ){
  out.title <- "Seroprevalence plot"
}else{
  if(outcome == "Day_of_Extinction"){
    out.title <- "Persistence plot"
  }
}

if(Q_biteA_Seroprev.mean == TRUE ){
  cu.title <- "Aedes bite rate"
}else{
  cu.title <- ""
}

if(Q_muC_Seroprev.mean == TRUE){
  cu.title <- "Culex mortality rate"
}else{
 cu.title <- ""
}


Datestamp <- format(Sys.Date(), "%Y_%m_%d")

time.title <- round(end.time/365,0)

var.title <- paste(var1, "vs", var2, sep = " ")

#filenames
plot.title <- paste(out.title, var.title, cu.title, "for", time.title, "years", Datestamp, "For Publication",sep = " ")

if(R0_plot==TRUE){
  plot.title <- paste("R0", plot.title, sep = " ")
}

data.file.title <- paste0("./All_Files_For_Publication/Data_for_sensitivity_analysis/", plot.title, ".Rdata")
csv.file.title <- paste0("./All_Files_For_Publication/Data_for_sensitivity_analysis/", plot.title, ".csv")

#Save
save(summary.dat, file = data.file.title )
write.csv(summary.dat, csv.file.title, row.names = FALSE)


print(paste0("Successfully completed at ", Sys.time()))

#Make Plots
if(Vax_Q_Persistence == TRUE ){ 
  ggplot(summary.dat, aes(x = PropVax, y = .data[[outcome]], color = .data[[var1]]))+
    geom_point()+
    geom_line()+
    scale_color_discrete(name = var1.plot, breaks = discrete_vec, labels = discrete_vec)+
    labs(x = var2.plot, y = outcome.plot)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          axis.title = element_text(colour = "black", size=12), 
          axis.text = element_text(colour = "black", size = 10))
}else{
  ggplot(summary.dat, aes(x = .data[[var2]], y = .data[[outcome]], color = .data[[var1]]))+
    geom_point()+
    geom_line()+
    labs(x = var2.plot, y = outcome.plot)+
    scale_colour_discrete(name  = var1.plot, breaks= discrete_vec, labels=discrete_vec)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          axis.title = element_text(colour = "black", size=12), 
          axis.text = element_text(colour = "black", size = 10))
}


if(R0_plot == TRUE){
  summary.dat[[var1]] <- as.factor(summary.dat[[var1]])
  summary.dat <- summary.dat%>%
    mutate(Persist_long = if_else(Persistence > 12165, "Yes", "No"))%>%
    mutate(Persist_year = if_else(R0_when_q0 >1, "Yes", "No"))
  
  R0.seroprev.plot <- ggplot(summary.dat, aes(x = mean_popn_R0, y = .data[[outcome]], color = .data[[var1]]))+
    geom_line(aes(linetype = Persist_year))+
    geom_point(aes(shape = Persist_long), size = 3)+
    scale_color_discrete(name = var1.plot, breaks = discrete_vec, labels = discrete_vec)+
    scale_shape_manual(name = "34-Year Persistance", values=c(4, 1))+
    scale_linetype_manual(name = "Within Year Persistence", values = c( "dashed","solid"), labels = c("Within Year Extinction Without Transovarial Transmission", "Within Year Persistence Without Transovarial Transmission"))  + 
    labs(x = expression(paste("R" [0])), y = outcome.plot)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          axis.title = element_text(colour = "black", size=12), 
          axis.text = element_text(colour = "black", size = 10))
  
  R0.seroprev.plot
  
  R0.seroprev.plot <- ggarrange(R0.seroprev.plot)
  
  ggexport(R0.seroprev.plot, filename = paste0("./All_Files_For_Publication/Data_for_sensitivity_analysis/", plot.title, ".png"), width=1004, height=601, ncol = 1,nrow = 1)
  
}else{
  png.file.title <- paste0("./All_Files_For_Publication/Data_for_sensitivity_analysis/", plot.title, ".png")
  ggsave(png.file.title)
}


print("Completed")

