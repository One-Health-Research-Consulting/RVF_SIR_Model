#' ---
#' title: "Making plots of the RVFV SIRS Model"
#' author: "Mindy Rostal"
#' date: "December 11, 2024"
#' output: html_document
#' ---

# load libraries
library(egg)
library(ggplot2)
library(ggpubr)
library(grid)
library(here)
library(dplyr)
library(ggpattern)
library(emojifont)
library(tidyr)
library(ggtext)
library(ragg)
library(gridtext)
library(viridis)


#Load plotting functions
source(here("All_Files_For_Publication/Functions", "Function 3 Plot simulations of a given time period.R"))

#Set ggplot theme
plot_theme <- theme_classic() +
  theme(axis.text = element_text(size = 9, colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.title = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 9)) + 
  theme(legend.title = element_text(size = 12)) + 
  theme(plot.title = element_text(size=16))

########################################################
########################################################
#Full simulations

#Make ggplots to combine
#' _Aedes_ Mosquitoes
#' 
MosqAplot <-ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = SA, colour = "SA")) + 
  geom_line(aes(y = IA, colour = "IA")) +
  labs( x = "Year", y = "*Aedes*<br>Mosquitoes") + #= my_y_title.a) +  
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual(name = "Population", values =c("SA" = "black","IA"= "red"), labels = c("IA", "SA")) +
  plot_theme+
  theme(axis.title = element_markdown(size = 13, family = "sans"),
        legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
        axis.text = element_markdown(size = 9, family = 'sans'),
        legend.title = element_markdown(size = 10, family = 'sans'),
        plot.margin=unit(c(0,.5,0,.5),"cm"),
        legend.key.size = unit(.15, "cm")
  )


#' infected _Aedes_ eggs
MosqIAEplot <- ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = IAE, colour = "IAE")) + 
  labs(x = "Year", y = "Infected<br>*Aedes* Eggs") + #my_y_title.iae) +
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual(name = "Population", values =c("IAE" = "#8B4000")) +
  plot_theme+
  theme(plot.margin=unit(c(.5,0,0,.5),"cm"),
        axis.title = element_markdown(size = 13, family = "sans", margin = c(0, 0, 0, 3)),
        axis.text = element_markdown(size = 9, family = 'sans'),
        legend.key = element_rect(fill = "white", color = "white"), legend.text = element_text(color = "white"), legend.title = element_text(color = "white")) +
  guides(color = guide_legend(override.aes = list(color = NA)))

#' _Culex_ Mosquito Population
MosqCplot <- ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = SC, colour = "SC")) + 
  geom_line(aes(y = IC, colour = "IC")) +
  labs(x = "Year", y = "*Culex*<br>Mosquitoes") + #y = my_y_title.c) +
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual(name = "Population", labels = c("SC" = "SC", "IC" = "IC"), values =c("SC" = "black","IC"= "pink")) +
  plot_theme+
  theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
        axis.title = element_markdown(size = 13, family = "sans"),
        legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
        axis.text = element_markdown(size = 9, family = 'sans'),
        legend.title = element_markdown(size = 10, family = 'sans'),
        legend.key.size = unit(.15, "cm"))

#' Total Infected Mosquitoes
MosqIAll <-ggplot(final.populations, aes(time)) + 
  geom_line(aes(y = IA, colour = "IA")) + 
  geom_line(aes(y = IC, colour = "IC")) +
  geom_vline(xintercept = 9915, color = "dark blue")+#This is 3-1-2010
  labs( x = "Year", y = "Infected<br>Mosquitoes") + #y = my_y_title.all)+ 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
  scale_colour_manual(name = "Population", values =c("IA" = "red","IC"= "pink")) +
  plot_theme+
  theme(axis.title = element_markdown(size = 13, family = "sans", margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt')),
        axis.title.y = element_markdown(valign = 4),
        legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
        axis.text = element_markdown(size = 9, family = 'sans'),
        legend.title = element_markdown(size = 10, family = 'sans'),
        plot.margin=unit(c(0,.5,0,.5),"cm"),
        legend.key.size = unit(.15, "cm"))

#'Sheep and Lambs
SLplot <-ggplot(final.populations, aes(x=time)) + 
  geom_line(aes(y = SS, colour = "SS")) + 
  geom_line(aes(y = RS, colour = "RS")) +
  geom_line(aes(y = SL, colour = "SL")) + 
  geom_line(aes(y = RL, colour = "RL")) +
  geom_line(aes(y = AL, colour = "AL")) +
  geom_line(aes(y = IS, colour = "IS")) +
  geom_line(aes(y = IL, colour = "IL")) +
  geom_vline(xintercept = 9915, color = "dark blue")+#This is 2-15-2010
  labs( x = "Year", y = "Sheep<br>and Lambs") + #y = my_y_title.sl) +#, color = "Population") + 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 1, to = end.time, by = 365)) +
  scale_colour_manual( name = "Population", values =c("SS" = "black","IS"= "red", "RS" = "green", "SL" = "gray","IL"= "pink", "RL" = "light green", "AL" = "violet")) +
  plot_theme+
  theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
        axis.title = element_markdown(size = 13, family = "sans", margin = margin(t = -10, r = 0, b = 0, l = 0, unit = 'pt')),
        legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
        axis.text = element_markdown(size = 9, family = 'sans'),
        legend.title = element_markdown(size = 10, family = 'sans'),
        legend.key.size = unit(.15, "cm"))#decrease space between the legend items

#Sheep and lambs including vaccinated animals
SLplot_vax <-ggplot(final.populations, aes(x=time)) + 
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
  labs( x = "Year", y = "Sheep <br>and Lambs") + #y = my_y_title.sl) +#, color = "Population") + 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 1, to = end.time, by = 365)) +
  scale_colour_manual( name = "Population", values =c("SS" = "black","IS"= "red", "RS" = "green", "VS" = "blue", "SL" = "gray","IL"= "pink", "RL" = "light green", "VL" = "light blue", "AL" = "violet")) +
  plot_theme+
  theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
        axis.title = element_markdown(size = 13, family = "sans", margin = margin(t = -10, r = 0, b = 0, l = 0, unit = 'pt')),
        legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
        axis.text = element_markdown(size = 9, family = 'sans'),
        legend.title = element_markdown(size = 10, family = 'sans'),
        legend.key.size = unit(.15, "cm"))#decrease space between the legend items


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

SLplot_historical <- ggplot(final.populations, aes(x=time)) + 
  geom_line(aes(y = SS, colour = "SS")) + #No VS/VL
  geom_line(aes(y = IS, colour = "IS")) +
  geom_line(aes(y = RS, colour = "RS")) +
  geom_line(aes(y = SL, colour = "SL")) + 
  geom_line(aes(y = IL, colour = "IL")) +
  geom_line(aes(y = RL, colour = "RL")) +
  geom_line(aes(y = AL, colour = "AL")) +
  geom_vline(xintercept = 9915, color = "dark blue")+#This is 2-15-2010
  labs( x = "Year", y = "Sheep<br>and Lambs") +#, color = "Population") + 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 1, to = end.time, by = 365)) +
  scale_colour_manual( name = "Population", values =c("SS" = "black","IS"= "red", "RS" = "green", "SL" = "gray","IL"= "pink", "RL" = "light green", "AL" = "violet")) +
  plot_theme+
  theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
        axis.title = element_markdown(size = 13, family = "sans"),
        legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
        axis.text = element_markdown(size = 9, family = 'sans'),
        legend.title = element_markdown(size = 10, family = 'sans'),
        legend.key.size = unit(.15, "cm")) + 
  geom_text(aes(x = HV_1983, y = 450, label = "\U2605"), size=4)+  
  geom_text(aes(x = HV_1985, y = 450, label = "\U2605"), size=4)+  
  geom_text(aes(x = HV_2009, y = 450, label = "\U2605"), size=4)+  
  geom_text(aes(x = HV_2010, y = 450, label = "\U2605"), size=4)+  
  geom_text(aes(x = U_1989,  y = 450, label = "\U2605"), size=4, color = "gray")+  
  geom_text(aes(x = U_1987,  y = 450, label = "\U2605"), size=4, color = "gray")+ 
  geom_text(aes(x = HV_2011, y = 450, label = "\U2605"), size=4) #+
#theme(plot.margin=unit(c(.5,.5,0,.5),"cm"))

#Plot effective R0
Reffect.plot <- ggplot(final.populations, aes(x=time, y = Reff)) +
  geom_line(aes(color = "cheese")) +
  labs( x = "Year", y = "<br>Effective R<sub>0<sub> ") +#y = my_y_title.eR) + 
  scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end, by = 365)) +
  ylim(0,4) + 
  plot_theme +
  scale_color_manual(name = "che", values = c("cheese" = "cornflowerblue"))+ 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        axis.title = element_markdown(size = 13, family = "sans"),
        axis.text = element_markdown(size = 9, family = 'sans'))


########################################################
#Make plots for Fig 1, S3, portion of Fig S10 
#Runs if no scenarios were selected
if(muC.25_lower == FALSE & muC.25_higher == FALSE & vax.25_higher ==  FALSE & vax.25_lower == FALSE & lo.eggs == FALSE & no.amp.vecs == FALSE & No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE){

    
    #Figure 1 - remove all legends then make a legend for the bottom of the page
    #Set all legends to zero
    SLplot_historical_nl <- SLplot_historical + theme(legend.position = "none")
    MosqIAll_nl <-  MosqIAll  + theme(legend.position = "none")
    MosqAplot_nl <- MosqAplot + theme(legend.position = "none")
    MosqCplot_nl <- MosqCplot + theme(legend.position = "none")
    MosqIAEplot_nl <- MosqIAEplot + theme(legend.position = "none") #These two already didn't have legends, but had added a white box to make the plot the same size as the others.
    Reffect.plot_nl <- Reffect.plot + theme(legend.position = "none")
    
    #Make figure
    fil.name <- "Publication_Figures/Fig 1 Full Simulation SL IM IAE_Aedes SA-IA SC-IC and Re_no_legend.pdf"
    Fig1 <- ggarrange(SLplot_historical_nl, MosqIAll_nl, MosqIAEplot_nl, MosqAplot_nl, MosqCplot_nl,  Reffect.plot_nl, draw = FALSE,
                      labels = c("A", "B", "C", "D", "E", "F"),
                      heights = c(1.3,1,1,1,1,1.5),
                      align = "v", 
                      ncol = 1, nrow = 6)
    ggexport(Fig1[1], filename = fil.name, width=5, height=8)
    
    #Make a legend to be added to the bottom of the plot via another software system
    SLplot_legend_no_vax <- SLplot_historical + 
      scale_colour_manual(name = "Host population:", breaks = c("SS", "SL", "IS", "IL", "RS", "RL", "AL"), 
                          labels = c("SS" = "Susceptible<br>sheep","IS"= "Infected<br>sheep", "RS" = "Recovered<br>sheep", "SL" = "Susceptible<br>lambs","IL"= "Infected<br>lambs", "RL" = "Recovered<br>lambs", "AL" = "Lambs with<br>maternal antibodies"), 
                          values =c("SS" = "black","IS"= "red", "RS" = "green", "SL" = "gray","IL"= "pink", "RL" = "light green", "AL" = "violet")) + 
      #scale_size_manual(values = c(rep(1, 7))) +
      theme(legend.position = "bottom",
            legend.title = element_markdown(size=12),# valign = "middle"),
            legend.key.width = unit(3,"line"),
            legend.text = element_markdown(size=9, vjust = 10),  #valign = "middle") valign crashes r??
            legend.title.position = "top",
            plot.margin = unit(c(0,0,0,0),"cm"))
    
    # Extract the legend. Returns a gtable
    SL_leg <- get_legend(SLplot_legend_no_vax)
    # Convert to a ggplot and print
    sheep_legend <-  as_ggplot(SL_leg)
    
    ggsave(sheep_legend, filename = "Publication_Figures/sheep_legend_no_vax.pdf", width=8, height=1.3)
    
    #Mosquito legend
    Mozzie_legend <- MosqIAll +
      geom_line(aes(y = SC, colour = "SC")) + 
      scale_colour_manual(name = "Vector Population", values =c("IA" = "red","IC"= "pink", "SC" = "black"),
                          labels = c("SC" = "Susceptible *Aedes* or *Culex*","IA"= "Infected *Aedes*", "IC" = "Infected *Culex*")) +
      theme(legend.position = "bottom",
            legend.title = element_markdown(size=12),
            legend.key.width = unit(3,"line"),
            legend.text = element_markdown(size=9),#, vjust = 0.5),  #valign = "middle") valign crashes r??
            legend.title.position = "top",
            plot.margin= margin(b = 50, unit = "pt"))
    
    # Extract the legend. Returns a gtable
    mos_leg <- get_legend(Mozzie_legend)
    # Convert to a ggplot and print
    mosquito_legend <-  as_ggplot(mos_leg)
    
    ggsave(mosquito_legend, filename = "Publication_Figures/mosquito_legend_IA_IC_SC.pdf", width=8, height=1.3)
    
    #Mosquito legend
    Mozzie_legend_infect <- MosqIAll +
      scale_colour_manual(name = "Vector Population", values =c("IA" = "red","IC"= "pink"),
                          labels = c("IA"= "Infected *Aedes*", "IC" = "Infected *Culex*")) +
      theme(legend.position = "bottom",
            legend.title = element_markdown(size=12),
            legend.key.width = unit(3,"line"),
            legend.text = element_markdown(size=9),  #valign = "middle") valign crashes r??
            legend.title.position = "top",
            plot.margin= margin(b = 50, unit = "pt"))
    
    # Extract the legend. Returns a gtable
    mos_leg_i <- get_legend(Mozzie_legend_infect)
    # Convert to a ggplot and print
    mosquito_legend_inf <-  as_ggplot(mos_leg_i)
    
    ggsave(mosquito_legend_inf, filename = "Publication_Figures/mosquito_legend_infecteds_only.pdf", width=8, height=1.3)
    
    ######
    #Figure S3 Plot annual difference between infected culex and infected Aedes peaks (max(IC) - max(IA)) by number of infected hosts
    
    PeakIMosq_Plot <- ggplot(PeakInfectedMosq, aes(x = Ratio_iCu_to_iAe, y = Endemic), label = MosqYear +1 )+ #Add 1 to the Mosq year to get the actual year for the spring of the mosquito season
      geom_point()+ 
      lims(x=c(0,7))+
      geom_text(aes(label=ifelse(Ratio_iCu_to_iAe>2, MosqYear +1 , "")), hjust = -0.35,vjust = 0.35) +
      geom_text(aes(label=ifelse(MosqYear == 2008, MosqYear +1 , "")), hjust=-.35,vjust=1) +
      labs( x = expression(paste("Ratio of Infected ", italic("Culex"), " to Infected ", italic("Aedes"), " Populations")), y = "Infected Animals")+ 
      plot_theme + 
      theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
    
    FigS3 <- ggarrange(PeakIMosq_Plot, draw = FALSE, ncol = 1, nrow = 1)
    
    fil.nameS3 <- "Publication_Figures/Fig S3 Annual ratio of infected Culex and infected aedes populations by number of infected animals.pdf"
    
    ggexport(FigS3[1], filename = fil.nameS3, width=5, height=2.5)
    
    #########Figure 9B  
    
    SLplot_nl <- SLplot + 
      theme(legend.position = "none")
    
    fil.name.muC.norm <- "Publication_Figures/Fig S9B Full Simulation exemplar muC_no_legend.pdf"
    
    FigS9B <- ggarrange(SLplot_nl, MosqIAll_nl,  MosqIAEplot_nl, Reffect.plot_nl, draw = FALSE,
                        labels = c("A", "B", "C", "D"),
                        heights = c(1.25,1.25,1.2,1.25),
                        align = "v", 
                        ncol = 1, nrow = 4)
    
    
    ggexport(FigS9B[1], filename = fil.name.muC.norm, width=5, height=7)
    
  
  #Combine FigS9 with the two FigS9s already made (+/-50% biteC)
  if(file.exists("./Publication_Figures/Fig S9A Full Simulation Culex Death Rate Lower by 25 Percent_no_Lengend for publication.pdf") & file.exists("Publication_Figures/Fig S9B Full Simulation exemplar muC_no_legend.pdf") & file.exists("./Publication_Figures/Fig S9C Full Simulation Culex Death Rate Higher by 25 Percent_no_lengend for publication.pdf")){
    rl <- lapply(list("./Publication_Figures/Fig S9A Full Simulation Culex Death Rate Lower by 25 Percent_no_Lengend for publication.pdf", "Publication_Figures/Fig S9B Full Simulation exemplar muC_no_legend.pdf",  "./Publication_Figures/Fig S9C Full Simulation Culex Death Rate Higher by 25 Percent_no_lengend for publication.pdf"), magick::image_read_pdf)
    gl <- lapply(rl, grid::rasterGrob)
    ml <- marrangeGrob(gl, nrow = 1, ncol = 3, top=NULL, labels = c("A", "B", "C"))
    ml
    ggsave("./Publication_Figures/Fig S9 Combined Sims with various death rates_no_legend.pdf", ml)
  }
}
  

  #Figure S9a for a 25% lower Culex mortality rate
  if(muC.25_lower == TRUE & muC.25_higher == FALSE  & no.amp.vecs == FALSE & No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE & lo.eggs == FALSE ){
    fil.name.bite.lo <- "./Publication_Figures/Fig S9A Full Simulation Culex Death Rate Lower by 25 Percent_no_Lengend for publication.pdf"
    
    MosqIAll_nl <-  MosqIAll  + theme(legend.position = "none")
    MosqIAEplot_nl <- MosqIAEplot + theme(legend.position = "none") #These two already didn't have legends, but had added a white box to make the plot the same size as the others.
    Reffect.plot_nl <- Reffect.plot + theme(legend.position = "none")
    SLplot_nl <- SLplot + theme(legend.position = "none")
   
     

    FigS9A <- ggarrange(SLplot_nl, MosqIAll_nl,  MosqIAEplot_nl, Reffect.plot_nl, draw = FALSE,
                        labels = c("A", "B", "C", "D"),
                        align = "hv", 
                        heights = c(1.25,1.25,1.2,1.25),
                        ncol = 1, nrow = 4)
    
    ggexport(FigS9A[1], filename = fil.name.bite.lo, width=5, height=7)
    
  }else{
    #Figure S9c plot the 25% higher Culex mortality rate
    if( muC.25_higher == TRUE & muC.25_lower == FALSE & no.amp.vecs == FALSE & No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE & lo.eggs == FALSE ){
      
      fil.name.muC.hi <- "./Publication_Figures/Fig S9C Full Simulation Culex Death Rate Higher by 25 Percent_no_lengend for publication.pdf"
      MosqIAll_nl <-  MosqIAll  + theme(legend.position = "none",
                                        plot.margin=unit(c(top = 0, right = 0.5,bottom = 0, left=0.5),"cm"))
      MosqIAEplot_nl <- MosqIAEplot + theme(legend.position = "none") #These two already didn't have legends, but had added a white box to make the plot the same size as the others.
      Reffect.plot_nl <- Reffect.plot + theme(legend.position = "none")
      SLplot_nl <- SLplot + theme(legend.position = "none")
      
      FigS9C <- ggarrange(SLplot_nl, MosqIAll_nl,  MosqIAEplot_nl, Reffect.plot_nl, draw = FALSE,
                          labels = c("A", "B", "C", "D"),
                          align = "v", 
                          heights = c(1.25,1.25,1.2,1.25),
                          ncol = 1, nrow = 4)
      
      ggexport(FigS9C[1], filename = fil.name.muC.hi, width=5, height=7)
      
      
    }
  }


  
########################
#Make plots for Fig 2 R0 seroprev and persistence, S8 R0 for changing q and S7 as one or two parameters change
#This figure is only made if the main model was run with no scenarios.
if(muC.25_lower == FALSE & muC.25_higher == FALSE & vax.25_higher ==  FALSE & vax.25_lower == FALSE & lo.eggs == FALSE & no.amp.vecs == FALSE & No.q == FALSE & Only.q == FALSE & Vaccinate == FALSE){

 ######################   
    #Figure 2
    #R0 vs bite rate and q
    AeC_q_vs_biteA_R0 <- read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "R0 Seroprevalence plot q vs biteA  for 34 years 2025_02_11 For Publication.csv"))
    
    persist <- filter(AeC_q_vs_biteA_R0, Persist_long == "yes")
    #
    persist_q<- persist%>%
      group_by(q) %>% arrange(biteA) %>% 
      filter(row_number ()==1)%>% 
      mutate(first_persist = 1)%>%
      mutate(q = replace(q, q == 0.99, 1))
    
    
    persist_q_after_bite0.25 <- persist%>%
      filter( biteA >0.25)%>% #the curve up to bite A = 0.25 is captured by persist_q, after that it stays constant at q = 0.1
      filter( q == 0.1)
    
    persist_q <- rbind(persist_q, persist_q_after_bite0.25)
    
    persist.short <- filter(AeC_q_vs_biteA_R0, Persist_year == "yes")
    #
    persist_q.short<- persist.short%>%
      group_by(q) %>% arrange(biteA) %>% filter(row_number ()==1)%>% mutate(first_persist = 1)
    
 
    ##Get data from contour lines to highlight parameter space
    #Top contour - seroprevalence at 44%
    serop.conts <-     ggplot(data = AeC_q_vs_biteA_R0, aes(x = biteA, y = q))+
      geom_contour(aes(  z = Mean_Annual_Seroprevalence), colour = "white", alpha = 0.8, breaks = c(0.01, 0.05, seq(0.1,1, 0.1)))
    contours <-ggplot_build(serop.conts + geom_contour(aes(  z = Mean_Annual_Seroprevalence), breaks = c(0.01, 0.05, seq(0.1,1, 0.1))))$data[[2]]
    dat <- filter(contours, order == 0.40)
    dat <- dat%>%
      select( x, y)%>%
      mutate(y = replace(y, y == 0.99000000, 1))
    dat$data <- "ContourLine"
    
    #Bottom contour = above both seasonal R0=1 and persistence
    #There are three sections so need three polygons
    #get above R0=1
    R0_cont <-     ggplot(data = AeC_q_vs_biteA_R0, aes(x = biteA, y = q))+
      geom_contour(aes(  z = mean_popn_R0), colour = "white", alpha = 0.8, breaks = c(0.01, 0.05, seq(0.1,1, 0.1)))
    contours.r0 <-ggplot_build(R0_cont + geom_contour(aes(  z = mean_popn_R0), breaks = c(0.01, 0.05, seq(0.1,1, 0.1))))$data[[2]]
    dat.R0 <- filter(contours.r0, order == 1)
    dat.R0 <- select(dat.R0, x, y)
    dat.R0$data <- "R0Line"
    #Manually remove the points that are below the R0_cont line (q = 0.9, 0.8 and 0.7)
    dat.R0 <- filter(dat.R0, y <0.96 & y >0.7444736)
    
    
    #Get long term persistence
    line2 <- data.frame(matrix(nrow = 30, ncol = 3))
    names(line2) <- c("y", "x", "data")
    line2[,1:2] <- persist_q[,1:2]
    line2$data <- "Persist"
    #Manually remove the points that are below the R0_cont line (q = 0.9, 0.8 and 0.7)
    q.rm <- c(0.9, 0.8, 0.7)
    line2 <- filter(line2, !y %in% q.rm)
    
    #Combine to make bottom line of polygon
    fin.bot.dat <- data.frame(matrix(nrow = nrow(dat.R0)+ nrow(line2), ncol = 3))
    names(fin.bot.dat) <- c("y", "x", "data")
    fin.bot.dat <- rbind(line2,dat.R0)
    fin.bot.dat <- arrange(fin.bot.dat, desc(y))

    #Combine data for top and bottom lines
    dat1 <- bind_rows(dat, fin.bot.dat)
    
    #Two areas fill the region within the target seroprevalence and with persistence - we only want to include those areas.
    dat2 <- filter(dat1, x<=	0.1)
    dat3 <- filter(dat1, x>=0.11 & x <= 0.13)

    #Make dataframe for top and bottom of bracket

    #round q=0.99 to q = 1 
    AeC_q_vs_biteA_R0$q[AeC_q_vs_biteA_R0$q == 0.99] <- 1
    
    #Set up grid to indicate where the text should go
    text_df <- tibble(
      label = c(
        c("R<sub>0</sub> < 1", 
          "R<sub>0</sub> > 1",
          "Region of persistence<br>within observed<br>seroprevalence<br>range", 
          "Seasonal<br> R<sub>0</sub> < 1", 
          "Seasonal<br> R<sub>0</sub> > 1", 
          "Decadal<br>persistence", 
          "No<br>decadal<br>persistence")
        ),
      x = c(0.08, 0.185, 0.145, 0.05, 0.205, 0.195, 0.065 ),
      y = c(0.06, 0.06, 0.9, 0.6, 0.6, 0.43, 0.33 ),
      color = c("darkgreen", "darkgreen", "orange", "darkblue", "darkblue", "black", "black"),
      size = 8
    )
    
    
    Fig2.Seroprev.contour.plot <- ggplot(data = AeC_q_vs_biteA_R0, aes(x = biteA, y = q))+
      geom_contour_filled(aes(z = Mean_Annual_Seroprevalence), alpha = 0.5) +
      labs(#x = my_x_title.Abite,
           x = "*Aedes* Bite Rate",
           y = "Transovarial Transmission <br>Fraction<br>",
           fill = "Mean Seroprevalence") +
      #Fill contours
      scale_fill_manual(labels = c("0-5%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%"),
                        values = c("black","#3B528BFF",  "#2C728EFF", "#21908CFF", "#27AD81FF",   "#AADC32FF", "orange", "#FDE725FF", "#D5D0FC",   "#472D7BFF"))  + #Original first color: "#440154FF" "#5DC863FF", 
      ggnewscale::new_scale_fill()+      
      #Red shaded area indicating targer seroprevalence
      geom_contour_filled(aes( z = Mean_Annual_Seroprevalence), alpha = 0.9, fill = "violetred4", breaks = c(0.01, 0.40))+
      #White contour lines
      geom_contour(aes(x = biteA, y = q,  z = Mean_Annual_Seroprevalence), colour = "white", alpha = 0.5)+
      coord_fixed(ratio = 1/2) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      #Orange wedge that indicates the parameter space available
      geom_polygon_pattern(
        data = dat2, aes(x = x, y = y), fill = 'orange',  colour = 'orange', pattern_fill = "black")+#,, pattern = "circle",
      geom_polygon(
        data = dat3, aes(x = x, y = y), fill = 'orange')+#,  colour = 'orange')+#, pattern_fill = "black")+  
      #pattern_spacing = 0.05, pattern_density = 0.4) + 
      #R0 = 1
      geom_contour(aes(z = mean_popn_R0), colour = "cornflowerblue", breaks = 1, size = 1) +
      #seasonal R0 (q = 0) = 1
      geom_contour(aes(z = R0_when_q0), colour = "deeppink2", breaks = 1, size = 1) +
      #Persistence line
      geom_point(data = persist_q, aes(x = biteA, y = q), shape = 1, size = .5, colour = "chartreuse4") + 
      geom_line(data = persist_q, aes(x = biteA, y = q), colour = "chartreuse4", size = 1)+ 
      #Text
      geom_richtext(data = text_df, aes(x = x, y = y,
                    label = label, color = color), fill = NA, label.colour = NA, size = 3) +
      scale_colour_manual(values=c(  "black" = "chartreuse4", "darkblue" = "deeppink2", "darkgreen" = "cornflowerblue",  "orange" = "darkorange"), guide = "none") +
      #Annotate orange area
      annotate("segment", xend = 0.075, x = 0.1, yend = 0.8, y = 0.89, colour = "darkorange", size = 0.7, arrow=arrow(length = unit(0.3, "cm"))) +
      annotate("segment", xend = 0.17, x = 0.14, yend = 0.6, y = 0.6, colour = "deeppink2", size = 0.7, arrow=arrow(length = unit(0.3, "cm"))) +
      annotate("segment", xend = 0.08, x = 0.14, yend = 0.6, y = 0.6, colour = "deeppink2", size = 0.7, arrow=arrow(length = unit(0.3, "cm"))) +
      annotate("segment", xend = 0.16, x = 0.13, yend = 0.06, y = 0.06, colour = "cornflowerblue", size = 0.7, arrow=arrow(length = unit(0.3, "cm"))) +
      annotate("segment", xend = 0.10, x = 0.13, yend = 0.06, y = 0.06, colour = "cornflowerblue", size = 0.7, arrow=arrow(length = unit(0.3, "cm"))) +
      annotate("segment", xend = 0.16, x = 0.13, yend = 0.43, y = 0.4, colour = "chartreuse4", size = 0.7, arrow=arrow(length = unit(0.3, "cm"))) +
      annotate("segment", xend = 0.08, x = 0.13, yend = 0.35, y = 0.4, colour = "chartreuse4", size = 0.7, arrow=arrow(length = unit(0.3, "cm"))) +
      
      plot_theme+
      theme(axis.text = element_text(size = 10),
            axis.title = element_markdown(size = 12),
            #axis.title = element_text(size = 12),
            
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12))
    
    Fig2.Seroprev.contour.plot
    
  Fig2 <- ggarrange(Fig2.Seroprev.contour.plot, draw = FALSE,
                      ncol = 1, nrow = 1)
    fil.namefig2.pdf <- "./Publication_Figures/Fig 2 Seroprevalence contour while changing biteA and q with R0_must add target bracket.pdf"
     
    ggexport(Fig2, filename = fil.namefig2.pdf, width=7.38, height= 6.60)
     
######
    #Fig S8
    #Change to factor
    AeC_q_vs_biteA_R0$q <- as.factor(as.character(AeC_q_vs_biteA_R0$q))
    
    R0.seroprev.plot <- ggplot(AeC_q_vs_biteA_R0, aes_string(x = "mean_popn_R0", y = "Mean_Annual_Seroprevalence", color = "q"))+      geom_point(aes(shape = Persist_long), size = 1.5)+
      geom_line(aes(linetype = Persist_year))+
      geom_vline(xintercept = 1, color = "dark blue")+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.01, ymax = 0.4, 
               alpha = .3)+
      xlim(0.65,2.5)+ 
      scale_color_discrete(name = "Transovarial Transmission", breaks = unique(AeC_q_vs_biteA_R0$q), labels = unique(AeC_q_vs_biteA_R0$q))+
      scale_shape_manual(name = "34-Year Persistance", values=c(4, 1), labels = c("No", "Yes"))+
      scale_linetype_manual(name = "Seasonal Persistence", values = c( "dashed","solid"), labels = c("Seasonal extinction without \ntransovarial transmission", "Seasonal persistence without \ntransovarial transmission"))  + 
      labs(x = expression(paste("R" [0])), y = "Mean Annual \nSeroprevalence")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.title = element_text(colour = "black", size=12), 
            axis.text = element_text(colour = "black", size = 10),
            legend.position = "right")+
      guides(col = guide_legend(order = 3), shape = guide_legend(order = 2), linetype = guide_legend(order = 1)) #set legend order
    
    #
    R0.seroprev.plot.q0 <- ggplot(AeC_q_vs_biteA_R0, aes(x = R0_when_q0, y = Mean_Annual_Seroprevalence, color = q))+
      geom_point(aes(shape = Persist_long))+
      scale_shape_manual(name = "34-Year Persistance", values=c(4, 1), labels = c("No", "Yes"))+
      geom_line(aes(linetype = Persist_year))+
      scale_linetype_manual(name = "Seasonal Persistence", values = c( "dashed","solid"), labels = c("Seasonal extinction without transovarial transmission", "Seasonal persistence without transovarial transmission"))  + 
      scale_color_discrete(name = "Transovarial Transmission", breaks = unique(AeC_q_vs_biteA_R0$q), labels = unique(AeC_q_vs_biteA_R0$q))+
      geom_vline(xintercept = 1, color = "dark blue")+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.01, ymax = 0.4, 
               alpha = .3)+
      xlim(0.65,2.5)+ 
      labs(x = expression(paste("Seasonal R" [0])), y = "Mean Annual \nSeroprevalence")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.title = element_text(colour = "black", size=12), 
            axis.text = element_text(colour = "black", size = 10),
            legend.position = "right")+
      guides(col = guide_legend(order = 3), shape = guide_legend(order = 2), linetype = guide_legend(order = 1)) #set legend order
    
      FigS8 <- ggarrange(R0.seroprev.plot, R0.seroprev.plot.q0, draw = FALSE,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2,
                      common.legend = TRUE, legend = "right")
    fil.namefigS8 <- "./Publication_Figures/Fig S8 R0 and seroprevalence while changing biteA and q.pdf"
    ggexport(FigS8[1], filename = fil.namefigS8, width=6, height=5)    
    
###################    
    #Figure S7
    #Load data for seroprevalence and extinction single variable plots (A & B)
    AeC_q <- read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "q vs Mean Seroprevalence and Extinction Day.csv"))
    AeC_Tsla <- read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "Tsla vs Mean Seroprevalence and Extinction Day.csv"))
    AeC_Tasl <- read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "Tasl vs Mean Seroprevalence and Extinction Day.csv"))
    AeC_epsilon <- read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "epsilon vs Mean Seroprevalence and Extinction Day.csv"))
    
    All_single <- rbind(AeC_q,  AeC_Tsla, AeC_Tasl, AeC_epsilon)#AeC_biteA,
    
    All_single$Parameter_Tested <- factor(All_single$Parameter_Tested, levels = c("epsilon", "Tsla", "Tasl", "q"))#"biteA", 
    
    All_single <- All_single%>%
      mutate(Year = round(Extinction_Day/365,0))
    
    Plot.Extinction <- ggplot(All_single, aes(x=Parameter_Value, y = Year, color = Parameter_Tested))+
      geom_line()+
      geom_point()+
      scale_color_brewer(palette="Dark2", name = "Parameter", labels = c("Daily Extrinsic \nIncubation", "*Aedes*-to-Ruminant \nTransmission Fraction", "Ruminant-to-*Aedes* \nTransmission Fraction", "Transovarial Transmission \nFraction"))+
      labs(x = "Parameter Value", y ="Years of Persistence")+
      plot_theme +
      theme(axis.title = element_markdown(size = 13, family = "sans"),
            legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
            axis.text = element_markdown(size = 9, family = 'sans'),
            legend.title = element_markdown(size = 10, family = 'sans'),
            legend.key.size = unit(.4, "cm"),
            legend.text.align = 0,
            plot.margin=unit(c(t=0,r=6.5,0,l = 1),"cm"),
            legend.position = "inside", 
            legend.position.inside = c(.99, .5),
            legend.justification = c(.1, .25))
    
    Plot.SeroP <- ggplot(All_single, aes(x=Parameter_Value, y = Mean_Seroprevalence, color = Parameter_Tested))+
      geom_line()+
      geom_point()+
      scale_color_brewer(palette="Dark2", name = "Parameter", labels = expression(paste("Extrinsic Incubation (per Day)"), paste("Transmission ", italic("Aedes"), "-to-Ruminant Fraction"), paste("Transmission Ruminant-to-", italic("Aedes"), " Fraction"), paste("Transovarial Transmission Fraction")))+
      labs(x = "Parameter Value", y ="Mean Seroprevalence")+
      plot_theme+
      theme(legend.position = "none",
            plot.margin=unit(c(0,0,0,1),"cm"),
            axis.title = element_markdown(size = 13, family = "sans"),
            axis.text = element_markdown(size = 9, family = 'sans'))
    
    #Load data for Variable by variable plots (C & D)
    AeC_q_vs_biteA <- read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "Seroprevalence plot q vs biteA  for 34 years 2025_02_11 For Publication.csv"))
    AeC_q_vs_tasl_Persist <- read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "Persistence plot q vs Tasl  for 34 years 2025_02_11 For Publication.csv"))
    
    #Change to factor
    AeC_q_vs_biteA$q <- as.factor(as.character(AeC_q_vs_biteA$q))
    AeC_q_vs_tasl_Persist$q <- as.factor(as.character(AeC_q_vs_tasl_Persist$q))
    
    #Plot
    plot.7Sc <- ParamByParamPlot(df1 = AeC_q_vs_biteA, colx = "biteA", coly = "Mean_Annual_Seroprevalence", color1 = "q", no.leg = TRUE, R0 = FALSE)
    plot.7Sd <- ParamByParamPlot(df1 = AeC_q_vs_tasl_Persist, colx = "Tasl", coly = "Day_of_Extinction", "q", no.leg = FALSE, R0 = FALSE)
    
    plot.7Sc <- plot.7Sc+
      theme(plot.margin=unit(c(t=0,0,0,01),"cm"),
            axis.title = element_markdown(size = 13, family = "sans"),
            axis.text = element_markdown(size = 9, family = 'sans')
            )
    
    
    plot.7Sd <- plot.7Sd+
      theme(plot.margin=unit(c(t=0,0,0,1),"cm"),
            axis.title = element_markdown(size = 13, family = "sans"),
            axis.text = element_markdown(size = 9, family = 'sans'),
            legend.title = element_markdown(size = 10, family = 'sans'),
            legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
            legend.key.size = unit(.4, "cm"))
    #Combine plots in S5
    
    FigS7 <- ggarrange(Plot.SeroP, Plot.Extinction, plot.7Sc, plot.7Sd, ncol = 2, nrow = 2,
                      labels = c("A", "B", "C", "D"), align = "h", 
                      heights = c(300,300),  widths = c(1.5,2.5))
    fil.nameS7 <- "./Publication_Figures/Fig S7 Combined plots of single and dual variable effect on seroprevalence and persistance for publication.pdf"
    
    ggexport(FigS7, filename = fil.nameS7, width=9 , height=5)
    #ggexport(FigS5, filename = fil.nameS5, width=907 , height=834)
    

}



######################################################
#Plot scenarios
#Figure 4, S12 and S12- vaccination scenarios
if(Vaccinate == TRUE & no.amp.vecs == FALSE & No.q == FALSE & Only.q == FALSE & lo.eggs == FALSE & muC.25_lower == FALSE & muC.25_higher == FALSE){
  if(vax.25_higher == FALSE & vax.25_lower == FALSE ){ 
    #Save SLplot and MosqIAll plots separately so easier to compile
    #Fig4a
    SLplot_vax1 <- SLplot_vax + 
      theme(legend.position = "none", 
            plot.margin= margin(t = 5, b = 50, unit = "pt"))
    fil.name4a <- "Publication_Figures/Fig 4A Full Simulation SL_no Legend.pdf"
    Fig4a <- ggarrange(SLplot_vax1,
                           labels = c("A"),
                           heights = 2.5,
                           ncol = 1, nrow = 1)
    ggexport(Fig4a, filename = fil.name4a, width=5, height=2.5) #FigVaxSim[1]???
    
    #Fig4b
    fil.name4b <- "Publication_Figures/Fig 4B Full Simulation IM_no_legend.pdf"
    MosqIAll_vax <- MosqIAll + 
      theme(legend.position = "none", 
            plot.margin= margin(t = 5, b = 50, unit = "pt"))
    Fig4b <- ggarrange( MosqIAll_vax,
                           labels = c( "B"),
                           heights = 2.5,
                           ncol = 1, nrow = 1)
    ggexport(Fig4b, filename = fil.name4b, width=5, height=2.5) #FigVaxSim[1]???
    
    
    #Make a legend to be added to the bottom of the plot via another software system
    SLplot_legend_vax <- SLplot_vax + 
      scale_colour_manual(name = "Host population:", breaks = c("SS", "SL", "IS", "IL", "RS", "RL", "AL", "VS", "VL"), 
                          labels = c("SS" = "Susceptible<br>sheep","IS"= "Infected<br>sheep", "RS" = "Recovered<br>sheep", "SL" = "Susceptible<br>lambs","IL"= "Infected<br>lambs", "RL" = "Recovered <br>lambs", "AL" = "Lambs with <br>maternal antibodies", "VS" = "Vaccinated<br>sheep", "VL" = "Vaccinated<br>lambs"), 
                          values =c("SS" = "black","IS"= "red", "RS" = "green", "SL" = "gray","IL"= "pink", "RL" = "light green", "AL" = "violet", "VS" = "blue", "VL" = "light blue")) + 
      #scale_size_manual(values = c(rep(1, 7))) +
      theme(legend.position = "bottom",
            legend.title = element_markdown(size=12),
            legend.key.width = unit(3,"line"),
            legend.text = element_markdown(size=9),  #valign = "middle") valign crashes r??
            legend.title.position = "top",
            plot.margin = unit(c(0,0,0,0),"cm"))
    
    # Extract the legend. Returns a gtable
    SL_vax_leg <- get_legend(SLplot_legend_vax)
    # Convert to a ggplot and print
    Sheep_vax_legend <-  as_ggplot(SL_vax_leg)
    
    ggsave(Sheep_vax_legend, filename = "Publication_Figures/Vaccinated sheep_legend_vax.pdf", width=8, height=1.3)
    

    #Fig 4C diff in livestock infection persistence and Aedes egg infection persistence
     Vax_ipop_difs <-  read.csv(here("All_Files_For_Publication/Data_for_sensitivity_analysis", "Percent vax vs Persistence in host and vector.csv"))
    Vax_ipop_difs_yr <- Vax_ipop_difs%>%
      mutate(Extinction_Yr_in_Host = round(Extinction_Day_in_Host/365,0))%>%
      mutate(Extinction_Yr_in_Eggs = round(Extinction_Day_in_Eggs/365,0))%>%
      pivot_longer(starts_with("Extinction_Yr"), names_to = "Species", values_to = "Years")%>%
      mutate(Last_Yr_IAE_Over_14400 = round(Last_Day_IAE_Over_14400/365,0))%>%
      mutate(Last_Yr_IAE_Over_14400 = replace(Last_Yr_IAE_Over_14400, Species == "Extinction_Yr_in_Host", 0))%>%
      mutate(q_sp = paste(Species, q, sep = "_"))%>%
      mutate(Last_Yr_IAE_Over_14400 = if_else(Last_Yr_IAE_Over_14400 == 0 & str_detect(Species, "Host"), Years, Last_Yr_IAE_Over_14400))%>%
      mutate(q = if_else(q == 0.75, "Low", "High"))

    plot_vax_ipersist <- ggplot(Vax_ipop_difs_yr, aes(x = Proportion_Vaccinated)) +
      #geom_bar(aes( y = Last_Yr_IAE_Over_14400, fill = q_sp), stat = "identity", position = "dodge") +
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
      scale_pattern_manual(values = c("Low" = "stripe", "High" = "none"), labels = c(0.90, 0.75), name = "Transovarial \ntransmission \n fraction") +
      #scale_pattern_manual(values = c("Extinction_Yr_in_Eggs" = "stripe", "Extinction_Yr_in_Host" = "none"), labels = c("Suffcient", "Insufficent"), name = "Vector Population Capable \n of Starting Outbreak in \nSusceptible Hosts") +
      scale_fill_brewer(palette="Set2", name = "Extinction Year", labels = expression(paste( italic("Aedes"), " Eggs"), paste("Host"))) +
      labs(x = "Percent Vaccinated", y = "Year of \nExtinction") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.title = element_text(colour = "black", size=18), 
            axis.text = element_text(colour = "black", size = 14), 
            plot.margin= margin(l = 5, b = 50, unit = "pt")) + 
      guides(pattern = guide_legend(override.aes = list(fill = "white")),
             fill = guide_legend(override.aes = list(pattern = "none"))) 
    
   
    fil.name4c <- "Publication_Figures/Fig 4C diff Infected Pops.pdf"
    
    plot_vax_ipersist <- ggarrange( plot_vax_ipersist,
                             labels = c( "C"),
                             ncol = 1, nrow = 1)
    
    ggexport(plot_vax_ipersist, filename = fil.name4c, width=7, height=3.5, ncol = 1,nrow = 1) 
    
    #Combine Fig 4s already made
    rl2 <- lapply(list( "./Publication_Figures/Fig 4A Full Simulation SL_no Legend.pdf", "Publication_Figures/Fig 4B Full Simulation IM_no_legend.pdf", "Publication_Figures/Fig 4C diff Infected Pops.pdf"), magick::image_read_pdf)     
    gl2 <- lapply(rl2, grid::rasterGrob)
    ml2 <- marrangeGrob(gl2, nrow = 4, ncol = 1, top=NULL)
    ggsave("./Publication_Figures/Fig 4 Vaccination figure.pdf", ml2, width=5, height=8, units = "in" )
    


#################   
    #Figure S10b
   
  
   fil.nameS10part <- "./Publication_Figures/Fig S10b Vaccination simulation as simulated_no_legend.pdf"
    
    #Remove the legends for 10b
    SLplot_vax2 <- SLplot_vax  +
      theme(legend.position = "none") #+
 
    MosqIAEplot_vax <- MosqIAEplot + 
      theme(legend.position = "none")#,
    
    #Arrange the plots to save for any vaccination scenario
    FigS10part <- ggarrange(SLplot_vax2, 
                            MosqIAEplot_vax,
                            align = "v", 
                           labels = c("A", "B"), nrow = 2, heights = c(300, 300))
    
    #Save files
    ggexport(FigS10part, filename = fil.nameS10part, width=5 , height=4)
    
    
  }
  
    if(vax.25_higher == TRUE | vax.25_lower == TRUE ){ 
  
  if(vax.25_higher == TRUE){
    #
    fil.nameS10part <- "./Publication_Figures/Fig S10c Vaccination simulation 25 perct higher_no_legend.pdf"
  }
  
  if(vax.25_lower == TRUE){
    #FigS10A

    fil.nameS10part <- "./Publication_Figures/Fig S10a Vaccination simulation 25 perct lower_no legend.pdf"
  }
      #Remove the legends for 10b
      SLplot_vax2 <- SLplot_vax  +
        theme(legend.position = "none") #+
      
      MosqIAEplot_vax <- MosqIAEplot + 
        theme(legend.position = "none")#,
      
      #Arrange the plots to save for any vaccination scenario
      FigS10part <- ggarrange(SLplot_vax2, 
                              MosqIAEplot_vax,
                              align = "v", 
                              labels = c("A", "B"), nrow = 2, heights = c(300, 300))
      
      #Save files
      ggexport(FigS10part, filename = fil.nameS10part, width=5 , height=4)
      
}
  
  if(vax.25_higher == FALSE & vax.25_lower == FALSE ){  
    #Combine Fig S10B with the two Fig S10s already made
    rl2 <- lapply(list("./Publication_Figures/Fig S10a Vaccination simulation 25 perct lower_no legend.pdf", "./Publication_Figures/Fig S10b Vaccination simulation as simulated_no_legend.pdf",  "./Publication_Figures/Fig S10c Vaccination simulation 25 perct higher_no_legend.pdf"), magick::image_read_pdf)
    gl2 <- lapply(rl2, grid::rasterGrob)
    ml2 <- marrangeGrob(gl2, nrow = 1, ncol = 3, top=NULL)
    ggsave("./Publication_Figures/Fig S10 Combined Sims with various vaccination rates_no_legend.pdf", ml2, width=6.5, height=2.3, units = "in" )
    
  }
}

#########
    #New Fig S11
      
    
    if(no.amp.vecs == TRUE & Vaccinate == FALSE & No.q == FALSE & Only.q == FALSE & lo.eggs == FALSE & muC.25_lower == FALSE & muC.25_higher == FALSE){
      MosqIAll_nl <-  MosqIAll  + theme(legend.position = "none")
      MosqIAEplot_nl <- MosqIAEplot + theme(legend.position = "none") 
      SLplot_nl <- SLplot + theme(legend.position = "none")
      
    fil.nameS11 <- "Publication_Figures/Fig S11 Only Aedes transmission SL IM IAE_Aedes_no_legend.pdf"
    FigS11 <- ggarrange(SLplot_nl, MosqIAll_nl, MosqIAEplot_nl, draw = FALSE,
                       labels = c("A", "B", "C"),
                       align = "v", 
                       ncol = 1, nrow = 3)
    ggexport(FigS11[1], filename = fil.nameS11, width=5, height=7)
  }
   



###################
#Figure S6
if(Only.q == TRUE & Vaccinate == FALSE & no.amp.vecs == FALSE & No.q == FALSE & lo.eggs == FALSE & muC.25_lower == FALSE & muC.25_higher == FALSE ){
  #No horizontal transmission and no vertical transmission
  #First run No.q, it'll save the data, then run the Analysis_1 file to plot the only.q data and combine into Figure S6.
  #Run the simulation without horizontal transmission and combine with previously saved data for no q.
  #Plot together with no.q - no vertical transmission
  final.pop.no.q <- read.csv("All_Files_For_Publication/Data_for_sensitivity_analysis/Data from most recent sim with no q.csv")
  SLplot_no_q <-ggplot(final.pop.no.q, aes(x=time)) + 
    geom_line(aes(y = SS, colour = "SS")) + 
    geom_line(aes(y = RS, colour = "RS")) +
    geom_line(aes(y = SL, colour = "SL")) + 
    geom_line(aes(y = RL, colour = "RL")) +
    geom_line(aes(y = AL, colour = "AL")) +
    geom_line(aes(y = IS, colour = "IS")) +
    geom_line(aes(y = IL, colour = "IL")) +
    geom_vline(xintercept = 9915, color = "dark blue")+#This is 2-15-2010
    labs( x = "Year", y = "Sheep <br>and Lambs") +#, color = "Population") + 
    scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 1, to = end.time, by = 365)) +
    scale_colour_manual( name = "Population", values =c("SS" = "black","IS"= "red", "RS" = "green", "SL" = "gray","IL"= "pink", "RL" = "light green", "AL" = "violet")) +
    plot_theme+
    theme(plot.margin=unit(c(t = 0.5, r = 0.25, b=0, l = 0.5),"cm"),
          axis.title = element_markdown(size = 13, family = "sans",margin = c(0, 0, 0, 3)),
          axis.text = element_markdown(size = 9, family = 'sans'),
          #legend.key.size = unit(.15, "cm"),
          legend.position = "none")#decrease space between the legend items
  
  MosqIAEplot_no_q <- ggplot(final.pop.no.q, aes(time)) + 
    geom_line(aes(y = IAE), colour = "#8B4000") + 
    scale_x_continuous(labels = unique(final.populations$Year), breaks = seq(from = 2, to = end.time, by = 365)) +
    labs(x = "Year", y = "Infected<br>*Aedes* Eggs") + #my_y_title.iae) +
    plot_theme+
    theme(plot.margin=unit(c(t = 0.5, r = 0.25, b=0, l = 0.5),"cm"),
          axis.title = element_markdown(size = 13, family = "sans", margin = c(0, 0, 0, 3)),
          axis.text = element_markdown(size = 9, family = 'sans'),
          legend.position = "none")
    
  SLplot_no_horiz <- SLplot +
    theme(plot.margin=unit(c(t = 0.5, r = 0.25, b=0, l = 0.5),"cm"),
          legend.position = "none")

  MosqIAEplot_no_horiz <- MosqIAEplot +
    theme(plot.margin=unit(c(t = 0.5, r = 0.25, b=0, l = 0.5),"cm"),
          legend.position = "none")

  MosqIAll_no_horiz <- MosqIAll +
    theme(plot.margin=unit(c(t = 0.5, r = 0.25, b=0, l = 0.5),"cm"),
          legend.position = "none")

  FigS6 <- ggarrange(SLplot_no_q, MosqIAEplot_no_q, SLplot_no_horiz, MosqIAEplot_no_horiz, MosqIAll_no_horiz,
                    labels = c("A", "B", "C", "D", "E"), 
                    nrow = 5, heights = c(500, 500, 500, 500, 500), 
                    align = "hv" )
  
  fil.nameS6 <- "./Publication_Figures/Fig S6 Combined Sims with no q and no horizontal transmission_no_legend.pdf"
  
  
  ggexport(FigS6, filename = fil.nameS6, width=5, height=7)
  }

###################################################################
#Additional supplemental plots

###########################
#Figs S2 and S12
if(muC.25_lower == FALSE & muC.25_higher == FALSE & Vaccinate == FALSE & no.amp.vecs == FALSE & No.q == FALSE & Only.q == FALSE & lo.eggs == FALSE){

    
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
      geom_line(color = "#01cdfe")+
      labs( x = "Year", y = "Monthly Cumulative \nRainfall (mm)")+ 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+#, limits = c(as.Date("1983-01-01", "2017-05-01"))) +
      plot_theme+
      theme(plot.margin=unit(c(t=0.4,r = 0.4,.40,l = 1),"cm"))
    
    FigS2.Temp <- ggplot(Temp_plot, aes(x = Month_Year, y = Temp, group = 1))+
      geom_line(color = "#ff71ce")+
      labs( x = "Year", y = "Monthly Mean \nTemperature (C)")+ 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+#, limits = c(as.Date("1983-01-01", "2017-05-01"))) +
      plot_theme+
      theme(plot.margin=unit(c(t=0.4,r = 0.4,.40,l = 1),"cm"))
    
    
    FigS2 <- ggarrange(FigS2.Rain, FigS2.Temp, 
                       ncol = 1, nrow = 2, align = "v", labels = c("A", "B"))
    fil.name.rain <- "./Publication_Figures/Fig S2 Rainfall and Temp during study period for publication.pdf"
    
    ggexport(FigS2, filename = fil.name.rain, width=5, height=4)

  #Effective R0 plot S14 - enlarged
    # Select a random year for this plot and the mosquito plot
    Myear<- final.populations%>%#Remove years with partial data
      filter(!MosqYear == "1982")%>%
      filter(!MosqYear == "1983")%>%
      filter(!MosqYear == "2017")%>%
      filter(!MosqYear == "2016")%>%
      group_by(MosqYear)%>%
      summarise(n=n())
    
    #Randomly selected mosquito population plots - Fig S12
    set.seed(6242015)
    
    #Select a random year and use the following two to visualize three years
    Myear.select <- sample_n(Myear, 1, replace = FALSE)
    
#See R0 analysis code for R0 sensitivity analysis and variable by variable sensitivity ()
    
    
    #Here just use the one year that was randomly selected
    Ran.Year <- final.populations%>%
      filter(MosqYear %in% Myear.select)
    
    Ran.Year$simtime <-  seq(1, 365, by = 1)
    
    Ran.Year <- Ran.Year%>%
      mutate(Month_Date = as.yearmon(paste(Ran.Year$Year, Ran.Year$Month), "%Y %m"))
    
    #Breaks and lables 
    break.labels <- unique(Ran.Year$Month_Date)
    
    #my_y_title.S12a <- expression(atop(italic("Aedes"), paste(" Population Size")))
    S12.Ae.Plot <-ggplot(Ran.Year, aes(simtime)) + 
      geom_line(aes(y = NAedes)) + 
      labs( x = "Month", y = "*Aedes* Population <br>Size") +      
      scale_x_continuous(labels = break.labels, breaks = c( 30, 61, 91, 122, 153, 181, 212, 242, 273, 303, 334, 365)) +
      plot_theme+
      theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
            axis.title = element_markdown(size = 13, family = "sans"),
            legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
            axis.text = element_markdown(size = 9, family = 'sans'),
            legend.title = element_markdown(size = 10, family = 'sans'),
            legend.key.size = unit(.15, "cm"))
    
    #################
    #Culex

    S12.Cu.Plot <-ggplot(Ran.Year, aes(simtime)) + 
      geom_line(aes(y = NC)) + 
      labs( x = "Month", y = "*Culex* Population <br>Size") +     
      scale_x_continuous(labels = break.labels, breaks = c( 30, 61, 91, 122, 153, 181, 212, 242, 273, 303, 334, 365)) +
      plot_theme+
      theme(plot.margin=unit(c(0,.5,0,.5),"cm"),
            axis.title = element_markdown(size = 13, family = "sans"),
            legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
            axis.text = element_markdown(size = 9, family = 'sans'),
            legend.title = element_markdown(size = 10, family = 'sans'),
            legend.key.size = unit(.15, "cm"))
    
    S12.MosqPop.Plot <- ggarrange(S12.Ae.Plot, S12.Cu.Plot, draw = FALSE,
                                 labels = c("A", "B"),
                                align = "v",
                                 ncol = 1, nrow = 2)
    
    fil.name.AeCu <- "./Publication_Figures/Fig S12 Aedes and Culex pops over one year for publication.pdf"
    
    ggexport(S12.MosqPop.Plot[1], filename = fil.name.AeCu, width=5, height=4)
 

}
