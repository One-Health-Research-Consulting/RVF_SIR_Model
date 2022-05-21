#' ---
#' title: "Making plots of the RVFV SIRS Model"
#' author: "Mindy Rostal"
#' date: "April 10, 2022"
#' output: html_document
#' ---

# load libraries
library(egg)
library(ggplot2)
library(ggpubr)
library(grid)
library(zoo)
library(png)
library(here)
library(dplyr)

#Set multiplot to TRUE to combine simulation plots and create most figures (e.g. Figure 1)
Multiplot <- TRUE

#Load plotting functions
source(here("Functions", "Function 3 Plot simulations of a given time period.R"))
source(here("Functions", "Function 7 Manage model outputs.R"))

model_run_path <- read_model_run_path()


#Set ggplot theme
plot_theme <- theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.title = element_text(size = rel(1.5))) + 
  theme(legend.text = element_text(size = 10)) + 
  theme(plot.title = element_text(size=16))

########################################################
########################################################
#Full simulations

#Make ggplots to combine
#' _Aedes_ Mosquitoes
MosqAplot <-ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = SA, colour = "SA")) + 
  geom_line(aes(y = IA, colour = "IA")) +
  labs( x = "Year", y = expression(italic("Aedes")), color = "Legend Title\n") +      
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual("", values =c("SA" = "black","IA"= "red")) +
  plot_theme

#' infected _Aedes_ eggs
my_y_title.iae <- expression(atop("Infected", paste(italic("Aedes"), " Eggs")))
#' 
MosqIAEplot <- ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = IAE, colour = "IAE")) + 
  labs(x = "Year",
    y = my_y_title.iae) +
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual("", values =c("IAE" = "red")) +
  plot_theme+
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) 

#' _Culex_ Mosquito Population
MosqCplot <- ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = SC, colour = "SC")) + 
  geom_line(aes(y = IC, colour = "IC")) +
  labs(x = "Year", y = expression(italic("Culex"))) +
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual("", values =c("SC" = "black","IC"= "red")) +
  plot_theme

#' Total Infected Mosquitoes
MosqIAll <-ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = IA, colour = "IA")) + 
  geom_line(aes(y = IC, colour = "IC")) +
  geom_vline(xintercept = 9915, color = "dark blue")+#This is 3-1-2010
  labs( x = "Year", y = "Infected \nMosquitoes")+ 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual("", values =c("IA" = "red","IC"= "pink")) +
  plot_theme+
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))

#'Sheep and Lambs
SLplot <-ggplot(final.populations, aes(x=time)) + 
  geom_line(aes(y = VS, colour = "VS")) +
  geom_line(aes(y = VL, colour = "VL")) +
  geom_line(aes(y = SS, colour = "SS")) + 
  geom_line(aes(y = RS, colour = "RS")) +
  geom_line(aes(y = SL, colour = "SL")) + 
  geom_line(aes(y = RL, colour = "RL")) +
  geom_line(aes(y = AL, colour = "AL")) +
  geom_line(aes(y = IS, colour = "IS")) +
  geom_line(aes(y = IL, colour = "IL")) +
  geom_vline(xintercept = 9915, color = "dark blue")+#This is 2-15-2010
  labs( x = "Year", y = "Sheep and \nLambs") + 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 1, to = end.time, by = 365)) +
  scale_colour_manual("", values =c("SS" = "black","IS"= "red", "RS" = "green", "VS" = "blue", "SL" = "gray","IL"= "pink", "RL" = "light green", "VL" = "light blue", "AL" = "violet")) +
  plot_theme+
  theme(plot.margin=unit(c(1,.5,.5,.5),"cm"),
        legend.key.size = unit(.25, "cm"))#decrease space between the legend items
  
#Plot Acutal outbreaks 1983-2017 from Pienaar et al
#Highveld/FS and central NC
HV_1983 <- which(final.populations$Year == 1983 & final.populations$Month == 04 & final.populations$MosqDay == 227) #15th; Highveld region
HV_1985 <- which(final.populations$Year == 1985 & final.populations$Month == 03 & final.populations$MosqDay == 196) #15th; Kroonstaad FS and Natal
HV_2009 <- which(final.populations$Year == 2009 & final.populations$Month == 11 & final.populations$MosqDay == 62)  #1st; Northern NC
HV_2010 <- which(final.populations$Year == 2010 & final.populations$Month == 04 & final.populations$MosqDay == 227) #15th; FS/NC,Gauteng, Limpopo, North West Province, EC, WC, Mpumalanga
HV_2011 <- which(final.populations$Year == 2011 & final.populations$Month == 02 & final.populations$MosqDay == 154) #15th; NC, EC and WC

#Elsewhere
EL_1983 <- as.Date("1983-11-15")#KZN and cape area of NC and WC
EL_1986 <- as.Date("1986-02-15")#Natal and EC
EL_1990 <- as.Date("1990-02-15")#Natal
EL_1999 <- as.Date("1999-01-15")#Kruger
EL_2008 <- as.Date("2008-03-15")#Mpumalanga, Gauteng, North West Province
EL_2009 <- as.Date("2009-04-15")#Limpopo, KZN, EC
EL_2008 <- as.Date("2008-03-15")#Limpopo, Gauteng, North West Province

#Unknown
U_1987 <- which(final.populations$Year == 1987 & final.populations$Month == 10 & final.populations$MosqDay == 45)  #15th; Unknown location
U_1989 <- which(final.populations$Year == 1989 & final.populations$Month == 02 & final.populations$MosqDay == 154) #15th; Unknown location

SLplot_historical <- SLplot+
  geom_text(aes(x = HV_1983, y = 500), label="★", size=5, family = "HiraKakuPro-W3")+  
  geom_text(aes(x = HV_1985, y = 500), label="★", size=5, family = "HiraKakuPro-W3")+  
  geom_text(aes(x = HV_2009, y = 500), label="★", size=5, family = "HiraKakuPro-W3")+  
  geom_text(aes(x = HV_2010, y = 500), label="★", size=5, family = "HiraKakuPro-W3")+  
  geom_text(aes(x = U_1989, y = 500), label="★", size=5, color = "gray", family = "HiraKakuPro-W3")+  
  geom_text(aes(x = U_1987, y = 500), label="★", size=5, color = "gray", family = "HiraKakuPro-W3")+ 
  geom_text(aes(x = HV_2011, y = 500), label="★", size=5, family = "HiraKakuPro-W3")

#Plot effective R0
Reffect.plot <- ggplot(final.populations, aes(x=time, y = Reff))+
  geom_line()+
  labs( x = "Year", y = "Effective R")+ 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end, by = 365)) +
  plot_theme + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))


########################################################
#Make plots for Fig 1, S3, portion of Fig S9 
#Runs if no scenarios were selected
if(muC.25_lower == FALSE & muC.25_higher == FALSE & Multiplot == TRUE & vax.25_higher ==  FALSE & vax.25_lower == FALSE 
   & No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE){
      #Figure 1
      fil.name <- "Publication_Figures/Fig 1 Full Simulation SL IM IAE_Aedes SA-IA SC-IC and Re.png"
      fil.name <- sprintf("%s/%s",model_run_path,fil.name)
      Fig1 <- ggarrange(SLplot_historical, MosqIAll, MosqIAEplot, MosqAplot, MosqCplot,  Reffect.plot, draw = FALSE,
                        labels = c("A", "B", "C", "D", "E", "F"),
                        ncol = 1, nrow = 6)
      
      ggexport(Fig1[1], filename = fil.name, width=593, height=915, ncol = 1,nrow = 6)
      
      #Figure S3 Plot annual difference between infected culex and infected Aedes peaks (max(IC) - max(IA)) by number of infected hosts
       PeakIMosq_Plot <- ggplot(PeakInfectedMosq, aes(x = Ratio_iCu_to_iAe, y = Endemic))+
        geom_point()+ 
        labs( x = expression(paste("Ratio of Infected ", italic("Culex"), " to Infected ", italic("Aedes"), " Populations")), y = "Infected Animals")+ 
        plot_theme + 
        theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
      
      FigS3 <- ggarrange(PeakIMosq_Plot, draw = FALSE, ncol = 1, nrow = 1)
      
      FigS3_path <- sprintf("%s/%s", model_run_path, "Publication_Figures/Fig S3 Annual ratio of infected Culex and infected aedes populations by number of infected animals.png")
      
      ggexport(FigS3[1], filename = FigS3_path, 
               width=593, height=230, ncol = 1, nrow = 1)
      
      
      #For Fig S9 - plot of the simulation Culex mortality rate
      fil.name.figS9b <- "Publication_Figures/Draft_Figures/Fig S9b Full Simulation Culex Death rate for publication.png"
        SLplot_noLeg <- SLplot + theme(legend.position = "none")
        MosqIAll_noLeg <- MosqIAll + theme(legend.position = "none")
        MosqIAEplot_noLeg <- MosqIAEplot + 
          ylim(0,160000) +
          theme(legend.position = "none")
        Reffect.plot <- Reffect.plot + ylim(0,6)
        
      FigS9b <- ggarrange(SLplot_noLeg, MosqIAll_noLeg, MosqIAEplot_noLeg, Reffect.plot, draw = FALSE,
                        labels = c("A", "B", "C", "D"),
                        ncol = 1, nrow = 4)
      
      
      fil.name.figS9b <-  sprintf("%s/%s",model_run_path,fil.name.figS9b)
      ggexport(FigS9b[1], filename = fil.name.figS9b, width=630, height=803, ncol = 1,nrow = 4)
    
      if(file.exists("./Publication_Figures/Draft_Figures/Fig S9A Full Simulation Culex Death Rate Lower by 25 Percent for publication.png") & file.exists("./Publication_Figures/Draft_Figures/Fig S9C Full Simulation Culex Death Rate Higher by 25 Percent for publication.png")){
  #Combine FigS9 with the two FigS9s already made (+/-50% biteC)
  rl <- lapply(list("./Publication_Figures/Draft_Figures/Fig S9A Full Simulation Culex Death Rate Lower by 25 Percent for publication.png", "Publication_Figures/Draft_Figures/Fig S9b Full Simulation Culex Death rate for publication.png",  "./Publication_Figures/Draft_Figures/Fig S9C Full Simulation Culex Death Rate Higher by 25 Percent for publication.png"), png::readPNG)
  gl <- lapply(rl, grid::rasterGrob)
  ml <- marrangeGrob(gl, nrow = 1, ncol = 3, top=NULL, labels = c("A", "B", "C"))
  ml
  ggsave("./Publication_Figures/Fig S9 Combined Sims with various death rates.png", ml)
}
}else{
  #Figure 9a for a 25% lower Culex mortality rate
  if(muC.25_lower == TRUE & Multiplot == TRUE){
    fil.name.bite.lo <- "./Publication_Figures/Draft_Figures/Fig S9A Full Simulation Culex Death Rate Lower by 25 Percent for publication.png"
  
    SLplot_noLeg <- SLplot + theme(legend.position = "none")
    MosqIAll_noLeg <- MosqIAll + theme(legend.position = "none")
    MosqIAEplot_noLeg <- MosqIAEplot + 
      ylim(0,160000) +
      theme(legend.position = "none")
    Reffect.plot <- Reffect.plot + ylim(0,6)
    
    FigS9A <- ggarrange(SLplot_noLeg, MosqIAll_noLeg,  MosqIAEplot_noLeg, Reffect.plot, draw = FALSE,
                       labels = c("A", "B", "C", "D"),
                       ncol = 1, nrow = 4)
    
    fil.name.bite.lo <- sprintf("%s/%s",model_run_path,fil.name.bite.lo)
    ggexport(FigS9A[1], filename = fil.name.bite.lo, width=630, height=803, ncol = 1,nrow = 4)
  }else{
    #Figure S9c plot the 25% higher Culex mortality rate
    if( muC.25_higher == TRUE & Multiplot == TRUE){
      
      fil.name.muC.hi <- "./Publication_Figures/Draft_Figures/Fig S9C Full Simulation Culex Death Rate Higher by 25 Percent for publication.png"
      
      SLplot_noLeg <- SLplot + theme(legend.position = "none")
      MosqIAll_noLeg <- MosqIAll + theme(legend.position = "none")
      MosqIAEplot_noLeg <- MosqIAEplot + 
        ylim(0,160000) + 
        theme(legend.position = "none")
    
      
      FigS9C <- ggarrange(SLplot_noLeg, MosqIAll_noLeg,  MosqIAEplot_noLeg, Reffect.plot, draw = FALSE,
                         labels = c("A", "B", "C", "D"),
                         ncol = 1, nrow = 4)
      
      fil.name.muC.hi <- sprintf("%s/%s", model_run_path, fil.name.muC.hi)
      ggexport(FigS9C[1], filename = fil.name.muC.hi, width=630, height=803, ncol = 1,nrow = 4)
    }else{
        print(MosqAplot)
        print(MosqIAEplot)
        print(MosqCplot)
        print(MosqIAll)
        print(SLplot)
    }
  }
}

#Make plots for Fig 2 R0 seroprev and persistence, S10 R0 for changing muC and q and S5 Seroprevalence and persistance by Variables
#This figure is only made if the main model was run with no scenarios.
if(Multiplot == TRUE & muC.25_lower == FALSE & muC.25_higher == FALSE & vax.25_higher ==  FALSE & vax.25_lower == FALSE){
  if(No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE){
    
    #Figure 2
    #R0 vs bite rate and q
    AeC_q_vs_biteA_R0 <- read.csv(here("Data for sensitivity analyses", "R0 Seroprevalence plot q vs biteA Aedes bite rate for 34 years 2021_05_05 For Publication.csv"))
     
    #Change to factor
    AeC_q_vs_biteA_R0$q <- as.factor(as.character(AeC_q_vs_biteA_R0$q))
  
    R0.seroprev.plot <- ggplot(AeC_q_vs_biteA_R0, aes_string(x = "mean_popn_R0", y = "Mean_Annual_Seroprevalence", color = "q"))+
      geom_line(aes(linetype = Persist_year))+
      geom_point(aes(shape = Persist_long), size = 3)+
      geom_vline(xintercept = 1, color = "dark blue")+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.05, ymax = 0.4, 
               alpha = .3)+
      xlim(0.65,2.5)+ 
      scale_color_discrete(name = "Transovarial Transmission", breaks = unique(AeC_q_vs_biteA_R0$q), labels = unique(AeC_q_vs_biteA_R0$q))+
      scale_shape_manual(name = "34-Year Persistance", values=c(4, 1), labels = c("No", "Yes"))+
      scale_linetype_manual(name = "Seasonal Persistence", values = c( "dashed","solid"), labels = c("Seasonal extinction without transovarial transmission", "Seasonal persistence without transovarial transmission"))  + 
      labs(x = expression(paste("R" [0])), y = "Mean Annual \nSeroprevalence")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.title = element_text(colour = "black", size=12), 
            axis.text = element_text(colour = "black", size = 10),
            legend.position = "right")
    
    R0.seroprev.plot.q0 <- ggplot(AeC_q_vs_biteA_R0, aes_string(x = "R0_when_q0", y = "Mean_Annual_Seroprevalence", color = "q"))+
      geom_line(aes(linetype = Persist_year))+
      geom_point(aes(shape = Persist_long), size = 3)+
      scale_shape_manual(name = "34-Year Persistance", values=c(4, 1), labels = c("No", "Yes"))+
      geom_vline(xintercept = 1, color = "dark blue")+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.05, ymax = 0.4, 
               alpha = .3)+
      xlim(0.65,2.5)+ 
      scale_color_discrete(name = "Transovarial Transmission", breaks = unique(AeC_q_vs_biteA_R0$q), labels = unique(AeC_q_vs_biteA_R0$q))+
      scale_linetype_manual(name = "Seasonal Persistence", values = c( "dashed","solid"), labels = c("Seasonal extinction without transovarial transmission", "Seasonal persistence without transovarial transmission"))  + 
      labs(x = expression(paste("Seasonal R" [0])), y = "Mean Annual \nSeroprevalence")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.title = element_text(colour = "black", size=12), 
            axis.text = element_text(colour = "black", size = 10),
            legend.position = "right")
    
    
    Fig2 <- ggarrange(R0.seroprev.plot, R0.seroprev.plot.q0, draw = FALSE,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2,
                      common.legend = TRUE, legend = "right")
    fil.namefig2 <- "./Publication_Figures/Fig 2 R0 and seroprevalence while changing biteA and q.png"
    
    fil.namefig2 <- sprintf("%s/%s", model_run_path,fil.namefig2)
    ggexport(Fig2[1], filename = fil.namefig2, width=630, height=503)
    
    #Fig S10 
    AeC_q_vs_muC_R0 <- read.csv(here("Data for sensitivity analyses", "R0 Seroprevalence plot q vs muC Culex mortality rate for 34 years 2022_04_04 For Publication.csv"))
    
    #Change to factor
    AeC_q_vs_muC_R0$q <- as.factor(as.character(AeC_q_vs_muC_R0$q))
    
    AeC_q_vs_muC_R0 <- AeC_q_vs_muC_R0%>%
      mutate(Persisted = if_else(Persistence > 12165, "Yes", "No"))%>%
      filter(!q== 0.75)#Too close to our value of 0.7, only need one.
    
    plot.R0.muC.q <- ggplot(AeC_q_vs_muC_R0, aes_string(x = "mean_popn_R0", y = "Mean_Annual_Seroprevalence", color = "q"))+
      geom_line(aes(linetype = Persist_year))+
      geom_point(aes(shape = Persist_long), size = 3)+
      geom_vline(xintercept = 1, color = "dark blue")+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.05, ymax = 0.4, 
               alpha = .3)+
      xlim(0.65,11)+ 
      scale_color_discrete(name = "Transovarial Transmission", breaks = unique(AeC_q_vs_muC_R0$q), labels = unique(AeC_q_vs_muC_R0$q))+
      scale_shape_manual(name = "34-Year Persistance", values=c(4, 1), labels = c("No", "Yes"))+
      scale_linetype_manual(name = "Seasonal Persistence", values = c( "dashed","solid"), labels = c("Seasonal extinction without transovarial transmission", "Seasonal persistence without transovarial transmission"))  + 
      labs(x = expression(paste("R" [0])), y = "Mean Annual \nSeroprevalence")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.title = element_text(colour = "black", size=12), 
            axis.text = element_text(colour = "black", size = 10),
            legend.position = "right")
    
    
    FigS10 <- ggarrange(plot.R0.muC.q, draw = FALSE,
                      ncol = 1, nrow = 1)
    
    fil.namefigS10 <- "./Publication_Figures/Fig S10 R0 and seroprevalence while changing muC and q.png"
    
    fil.namefigS10 <- make_file_name(model_run_path,fil.namefigS10)
    fil.namefigS10 <- make_file_name(model_run_path,fil.namefigS10)
    ggexport(FigS10[1], filename = fil.namefigS10, width=630, height=403)
    
    
    #Figure S5
    #Load data for seroprevalence and extinction single variable plots (A & B)
    AeC_q <- read.csv(here("Data for sensitivity analyses", "q vs Mean Seroprevalence and Extinction Day.csv"))
    AeC_Tsla <- read.csv(here("Data for sensitivity analyses", "Tsla vs Mean Seroprevalence and Extinction Day.csv"))
    AeC_Tasl <- read.csv(here("Data for sensitivity analyses", "Tasl vs Mean Seroprevalence and Extinction Day.csv"))
    AeC_epsilon <- read.csv(here("Data for sensitivity analyses", "epsilon vs Mean Seroprevalence and Extinction Day.csv"))
    
    All_single <- rbind(AeC_q,  AeC_Tsla, AeC_Tasl, AeC_epsilon)#AeC_biteA,
    
    All_single$Parameter_Tested <- factor(All_single$Parameter_Tested, levels = c("epsilon", "Tsla", "Tasl", "q"))#"biteA", 
    
    All_single <- All_single%>%
      mutate(Year = round(Extinction_Day/365,0))
    
    Plot.Extinction <- ggplot(All_single, aes(x=Parameter_Value, y = Year, color = Parameter_Tested))+
      geom_line()+
      geom_point()+
      scale_color_brewer(palette="Dark2", name = "Parameter", labels = expression(paste("Extrinsic Incubation"), paste(italic("Aedes"), " Bite Rate"), paste("Transmission ", italic("Aedes"), "-to-Ruminants"), paste("Transmission Ruminants-to-", italic("Aedes")), paste("Transovarial Transmission")))+
      labs(x = "Parameter Value", y ="Years of Persistence")+
      plot_theme +
      theme(legend.text.align = 0)
    
    Plot.SeroP <- ggplot(All_single, aes(x=Parameter_Value, y = Mean_Seroprevalence, color = Parameter_Tested))+
      geom_line()+
      geom_point()+
      scale_color_brewer(palette="Dark2", name = "Parameter", labels = expression(paste("Extrinsic Incubation"), paste(italic("Aedes"), " Bite Rate"), paste("Transmission ", italic("Aedes"), "-to-Ruminants"), paste("Transmission Ruminants-to-", italic("Aedes")), paste("Transovarial Transmission")))+
      labs(x = "Parameter Value", y ="Mean Seroprevalence")+
      plot_theme+
      theme(legend.position = "none")#+
    
    #Load data for Variable by variable plots (C & D)
    AeC_q_vs_biteA <- read.csv(here("Data for sensitivity analyses", "Seroprevalence plot q vs biteA  for 34 years 2022_05_10 For Publication.csv"))
    AeC_q_vs_tasl_Persist <- read.csv(here("Data for sensitivity analyses", "Persistence plot q vs Tasl  for 34 years 2022_05_09 For Publication.csv"))
    
    #Change to factor
    AeC_q_vs_biteA$q <- as.factor(as.character(AeC_q_vs_biteA$q))
    AeC_q_vs_tasl_Persist$q <- as.factor(as.character(AeC_q_vs_tasl_Persist$q))
    
    #Plots
    plot.5Sc <- ParamByParamPlot(AeC_q_vs_biteA, "biteA", "Mean_Annual_Seroprevalence", "q", no.leg = TRUE)
    plot.5Sd <- ParamByParamPlot(AeC_q_vs_tasl_Persist, "Tasl", "Day_of_Extinction", "q", no.leg = FALSE)
    
    #Combine plots in S5
    
    FigS5 <- ggarrange(Plot.SeroP, Plot.Extinction, plot.5Sc, plot.5Sd, ncol = 2, nrow = 2,
                      labels = c("A", "B", "C", "D"), align = "h", 
                      heights = c(300,300),  widths = c(1.5,2.5))
    fil.nameS5 <- "./Publication_Figures/Fig S5 Aedes and Culex - Plot of single variable effect on seroprevalence and persistance for publication.png"
    fil.nameS5 <- make_file_name(model_run_path,fil.nameS5)
    ggexport(FigS5, filename = fil.nameS5, width=907 , height=834)
    
  }
}



######################################################
#Plot scenarios
#Figure 3 and S11 - vaccination scenarios and S4 restricted transovarial and horizontal transmission scenarios
if(Vaccinate == TRUE){
  if(vax.25_higher == FALSE & vax.25_lower == FALSE){ 
    AeC_q_vs_vax <-  read.csv(here("Data for sensitivity analyses", "Persistence plot q vs vax  for 34 years 2022_05_10 For Publication.csv"))
    AeC_q_vs_vax$q <- as.factor(as.character(AeC_q_vs_vax$q))
    
    plot.3c <- ParamByParamPlot(AeC_q_vs_vax, "vax", "Day_of_Extinction", "q", no.leg = FALSE)
    
    Fig3 <- ggarrange(SLplot, 
                      MosqIAEplot,
                      ggarrange(plot.3c, ncol = 1, nrow = 1,
                                labels = c("C", NA), align = "h", 
                                heights = c(300)),
                      labels = c("A", "B", NA), nrow = 3, 
                      heights = c(300, 300, 300))
    
    fil.name3 <- "./Publication_Figures/Fig 3 Vaccination simulation and sensitivity to transovarial transmission.png"
    fil.name3 <- make_file_name(model_run_path,fil.name3)
    ggexport(Fig3, filename = fil.name3, width=907 , height=933)
    
    #Figure S11b
    
    SLplot <- SLplot + theme(legend.position = "none") +
      theme(axis.text = element_text(size = 14, colour = "black")) +
      theme(axis.title = element_text(size = 20)) 
    
    MosqIAEplot <- MosqIAEplot + theme(legend.position = "none") +
      theme(axis.text = element_text(size = 14, colour = "black")) +
      theme(axis.title = element_text(size = 20)) 
    
    fil.nameS11part <- "./Publication_Figures/Draft_Figures/Fig S11b Vaccination simulation as simulated.png"
  }
  
  if(vax.25_higher == TRUE){
    #Figure S11C
    SLplot <- SLplot + 
      theme(axis.text = element_text(size = 14, colour = "black")) +
      theme(axis.title = element_text(size = 20)) 
    
    MosqIAEplot <- MosqIAEplot +
      theme(axis.text = element_text(size = 14, colour = "black")) +
      theme(axis.title = element_text(size = 20)) 
    
    fil.nameS11part <- "./Publication_Figures/Draft_Figures/Fig S11c Vaccination simulation 25 perct higher.png"
  }
  
  if(vax.25_lower == TRUE){
    #FigS11A
    SLplot <- SLplot + theme(legend.position = "none") +
      theme(axis.text = element_text(size = 14, colour = "black")) +
      theme(axis.title = element_text(size = 20)) 
    
    MosqIAEplot <- MosqIAEplot + theme(legend.position = "none") +
      theme(axis.text = element_text(size = 14, colour = "black")) +
      theme(axis.title = element_text(size = 20)) 
    
    fil.nameS11part <- "./Publication_Figures/Draft_Figures/Fig S11a Vaccination simulation 25 perct lower.png"
  }
  
  
  #Arrange the plots to save for any vaccination scenario
  FigS11part <- ggarrange(SLplot, 
                         MosqIAEplot,
                         labels = c("A", "B"), nrow = 2, heights = c(300, 300))
  
  #Save files
  fil.nameS11part <- make_file_name(model_run_path,fil.nameS11part)
  ggexport(FigS11part, filename = fil.nameS11part, width=907 , height=933)
  
  if(file.exists("./Publication_Figures/Draft_Figures/Fig S11a Vaccination simulation 25 perct lower.png") & file.exists("./Publication_Figures/Draft_Figures/Fig S11b Vaccination simulation as simulated.png") & file.exists("./Publication_Figures/Draft_Figures/Fig S11c Vaccination simulation 25 perct higher.png")){  
    #Combine Fig S11B with the two Fig S11 already made
    rl2 <- lapply(list("./Publication_Figures/Draft_Figures/Fig S11a Vaccination simulation 25 perct lower.png", "./Publication_Figures/Draft_Figures/Fig S11b Vaccination simulation as simulated.png",  "./Publication_Figures/Draft_Figures/Fig S11c Vaccination simulation 25 perct higher.png"), png::readPNG)
    gl2 <- lapply(rl2, grid::rasterGrob)
    ml2 <- marrangeGrob(gl2, nrow = 1, ncol = 3, top=NULL)
    ggsave("./Publication_Figures/Fig S11 Combined Sims with various vaccination rates.png", ml2, width=6.5, height=2.5, units = "in" )
  }
}

#Figure S4 scenarios excluding transovarial or horizontal transmission
if(No.q == TRUE ){
  #No Transovarial transmission
  FigS4ab<- ggarrange(SLplot, MosqIAEplot,
                      labels = c("A", "B"), nrow = 2, heights = c(300, 300))
  
  fil.nameS4ab <- "./Publication_Figures/Draft_Figures/Fig S4A and B Plot of No Transovarial transmission.png"
  fil.nameS4ab <- make_file_name(model_run_path,fil.nameS4ab)
  ggexport(FigS4ab, filename = fil.nameS4ab, width=630, height=400)
} 

if(Only.q == TRUE ){
  #No horizontal transmission
  FigS4c<- ggarrange(SLplot, MosqIAEplot, MosqIAll,
                     labels = c("C", "D", "E"), nrow = 3, heights = c(300, 300, 300))
  
  fil.nameS4c <- "./Publication_Figures/Draft_Figures/Fig S4b Aedes and Culex - Plot of No Horizontal transmission.png"
  fil.nameS4c <- make_file_name(model_run_path,fil.nameS4c)
  ggexport(FigS4c, filename = fil.nameS4c, width=630, height=400)
  
  if(file.exists("./Publication_Figures/Draft_Figures/Fig S4A and B Plot of No Transovarial transmission.png")){
  rl2 <- lapply(list("./Publication_Figures/Draft_Figures/Fig S4A and B Plot of No Transovarial transmission.png", "./Publication_Figures/Draft_Figures/Fig S4b Aedes and Culex - Plot of No Horizontal transmission.png"), png::readPNG)
  gl2 <- lapply(rl2, grid::rasterGrob)
  ml2 <- marrangeGrob(gl2, nrow = 2, ncol = 1, top=NULL)
  ggsave("./Publication_Figures/Fig S4 Combined Sims with no q and no horizontal transmission.png", ml2, width=3 , height=4, unit = "in")
  }
}

###################################################################
#Additional supplemental plots

#Fig S2
if(muC.25_lower == FALSE & muC.25_higher == FALSE & Multiplot == TRUE & vax.25_higher ==  FALSE & vax.25_lower == FALSE){
  if(No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE){
    
    #Rainfall - supplemental
    All_Precip$Month <- as.character(All_Precip$Month)
    All_Precip$Month <-  str_pad(All_Precip$Month, width = 2, side = "left", pad = "0")
    All_Precip$Day <- as.character(All_Precip$Day)
    All_Precip$Day <-  str_pad(All_Precip$Day, width = 2, side = "left", pad = "0")
    
    PrecipTemp_plot <- All_Precip%>%
      mutate(Month_Year = paste( Year,Month,Day,  sep = "-"))
    
    Precip_plot <- PrecipTemp_plot%>%  
      group_by(Month_Year)%>%
      summarise(Rainfall = sum(DailyRainfall, na.rm = TRUE))%>%
      mutate(Year = str_sub(Month_Year, -4,-1))%>%
      arrange(Month_Year)
    
    Precip_plot$Month_Year <- as.Date(Precip_plot$Month_Year)
    
    Temp_plot <- PrecipTemp_plot%>%
      group_by(Month_Year)%>%
      summarise(Temp = mean(FinalMeanTemp, na.rm = TRUE))%>%
      mutate(Year = str_sub(Month_Year, -4,-1))%>%
      arrange(Month_Year)
    
    Temp_plot$Month_Year <- as.Date(Temp_plot$Month_Year)
    
    FigS2.Rain <- ggplot(Precip_plot, aes(x = Month_Year, y = Rainfall, group = 1))+
      geom_line(color = "blue")+
      labs( x = "Year", y = "Monthly Cumulative \nRainfall (mm)")+ 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+#, limits = c(as.Date("1983-01-01", "2017-05-01"))) +
      plot_theme+
      theme(plot.margin=unit(c(.40,.40,.40,.40),"cm"))
    
    FigS2.Temp <- ggplot(Temp_plot, aes(x = Month_Year, y = Temp, group = 1))+
      geom_line(color = "red")+
      labs( x = "Year", y = "Monthly Mean \nTemperature (C)")+ 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+#, limits = c(as.Date("1983-01-01", "2017-05-01"))) +
      plot_theme+
      theme(plot.margin=unit(c(.40,.40,.40,.40),"cm"))
    
    
    FigS2 <- ggarrange(FigS2.Rain, FigS2.Temp, 
                       ncol = 1, nrow = 2, labels = c("A", "B"))
    fil.name.rain <- "Publication_Figures/Fig S2. Rainfall and Temp during study period for publication.png"
    
    fil.name.rain<- make_file_name(model_run_path,fil.name.rain)
    ggexport(FigS2, filename = fil.name.rain, width=630, height=460, ncol = 1,nrow = 1)
  
    #Randomly selected mosquito population plots - Fig S6 & S12  
    Myear<- final.populations%>%#Remove years with partial data
      filter(!MosqYear == "1982")%>%
      filter(!MosqYear == "1983")%>%
      filter(!MosqYear == "2017")%>%
      filter(!MosqYear == "2016")%>%
      group_by(MosqYear)%>%
      summarise(n=n())
    
   
    
    Myear.select <- sample_n(Myear, 1, replace = FALSE)
    myY.select <- Myear.select$MosqYear
    Myears.select <- c(myY.select, myY.select+1, myY.select+2)
    
    
    Ran.Year <- final.populations%>%
      filter(MosqYear %in% Myear.select)
    Ran.Years <- final.populations%>%
      filter(MosqYear %in% Myears.select)
    
    Ran.Year$simtime <-  seq(1, 365, by = 1)
    Ran.Years$simtime <-  seq(1, 365+365+365, by = 1)
    
    Ran.Year <- Ran.Year%>%
      mutate(Month_Date = as.yearmon(paste(Ran.Year$Year, Ran.Year$Month), "%Y %m"))
    
    #Breaks and lables 
    break.labels <- unique(Ran.Year$Month_Date)
    break.labels.Re <- unique(Ran.Years$Year)
    break.labels.Re <- break.labels.Re[2:4] #Remove first MosqYear time
    break.seq <- seq(from = 123, to = nrow(Ran.Years), by = 365)
    
  #Effective R0 plot S6 - enlarged
    Reffect.Enlarge <- ggplot(Ran.Years, aes(x = simtime, y = Reff))+
      geom_line()+ 
      labs( x = "Year", y = "Effective R") +     
      ylim(NA,2)+
      scale_x_continuous(labels = break.labels.Re, breaks = break.seq) +
      plot_theme
    
    S6.Reffect.Plot <- ggarrange(Reffect.Enlarge)
    
    fil.name.S6Reff <- "./Publication_Figures/Fig S6 Effective R0 over three years.png"
    
    fil.name.S6Reff <- make_file_name(model_run_path,fil.name.S6Reff)
    
    ggexport(S6.Reffect.Plot, filename = fil.name.S6Reff, width=630, height=202, ncol = 1,nrow = 1)
    
#See R0 analysis code for R0 sensitivity analysis and variable by variable sensitivity ()
    
    
    
    my_y_title.S12a <- expression(atop(italic("Aedes"), paste(" Population Size")))
    S12.Ae.Plot <-ggplot(Ran.Year, aes(simtime)) + 
      geom_line(aes(y = NAedes)) + 
      labs( x = "Month", y = my_y_title.S12a) +      
      scale_x_continuous(labels = break.labels, breaks = c( 30, 61, 91, 122, 153, 181, 212, 242, 273, 303, 334, 365)) +
      plot_theme
    
    #################
    #Culex

    my_y_title.S3c <- expression(atop(italic("Culex"), paste(" Population Size")))
    S12.Cu.Plot <-ggplot(Ran.Year, aes(simtime)) + 
      geom_line(aes(y = NC)) + 
      labs( x = "Month", y = my_y_title.S3c) +      
      scale_x_continuous(labels = break.labels, breaks = c( 30, 61, 91, 122, 153, 181, 212, 242, 273, 303, 334, 365)) +
      plot_theme
    
    S12.MosqPop.Plot <- ggarrange(S12.Ae.Plot, S12.Cu.Plot, draw = FALSE,
                                 labels = c("A", "B"),
                                 ncol = 1, nrow = 2)
    
    fil.name.AeCu <- "./Publication_Figures/Fig S12. Aedes and Culex pops over one year for publication.png"
    fil.name.AeCu<- make_file_name(model_run_path,fil.name.AeCu)
    ggexport(S12.MosqPop.Plot[1], filename = fil.name.AeCu, width=630, height=402, ncol = 1,nrow = 4)
  
    # Start at same Randomly selected year for effective R plot - Fig S8    

    myY.select <- Myear.select$MosqYear
    Myears.select <- c(myY.select, myY.select+1, myY.select+2)
    #remove 
    Ran.Years <- final.populations%>%
      filter(MosqYear %in% Myears.select)
    
    Ran.Years$simtime <-  seq(1, 365+365+365, by = 1)
    
    #Breaks and lables 
    break.labels.Re <- unique(Ran.Years$Year)
    break.labels.Re <- break.labels.Re[2:4] #Remove first MosqYear time
    break.seq <- seq(from = 123, to = nrow(Ran.Years), by = 365)
    
      


    
 
    
    }
}

