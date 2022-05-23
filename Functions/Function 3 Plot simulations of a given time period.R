##########################
# Author: Mindy Rostal
# Date: 5/4/2022
##########################

##Functions for plotting outputs

source(here("Functions", "Function 7 Manage model outputs.R"))

###########################################################################
#1
ParamByParamPlot <- function(df1, colx, coly, color1, no.leg){
  
  discrete_vec <- as.factor(sort(unique(df1[[color1]])))
  discrete_vec2 <- discrete_vec
  
  if(no.leg ==TRUE){
    thesis_theme <- thesis_theme + theme(legend.position = "none")
  }
  
  if(color1 == "q"){
    df1[[color1]] <- as.factor(df1[[color1]])
    legend.name <- "Transovarial Transmission"
    color_vec <- c("salmon", "sandybrown", "mediumseagreen", "turquoise", "lightslateblue", "orchid1")
  }
  if(color1 == "vax"){
    legend.name = "Percent Vaccinated"
    discrete_vec2 <-  unique(1-exp(-df1[[color1]]*365))
    discrete_vec2 <- as.factor(as.character(round(discrete_vec2,2)))
    discrete_vec <- as.factor(as.character(unique(df1[[color1]])))
    df1[color1] <- as.factor(as.character(df1[, color1]))
  }  
  if(color1 == "epsilon"){
    legend.name = "Days of Extrinsic \n Incubation"
    df1 <- df1%>%
      mutate(dat1 = rev(epsilon), DaysInc = round("^"(epsilon, -1),0))
    color1 <- "DaysInc"
    discrete_vec2 <- as.factor(as.character(unique(df1[[color1]])))
    discrete_vec <- discrete_vec2
    df1[color1] <- as.factor(as.character(df1[, color1]))
    color_vec <- c("salmon", "sandybrown", "mediumseagreen", "turquoise", "lightslateblue", "orchid1", "lightcoral", "gray40")
  }
    
    if(color1 == "biteA"){
      df1.names <- colnames(df1)
        legend.name <- "Aedes Bite Rate"
    discrete_vec2 <- as.factor(as.character(unique(df1[[color1]])))
    df1[color1] <- as.factor(as.character(df1[, color1]))
    color_vec <- c("salmon", "sandybrown", "mediumseagreen", "turquoise", "lightslateblue", "orchid1")
  }
  
  #X axis numbers and names
  if(colx == "biteA"){
      df1.names <- colnames(df1)
      x.lab.name <- expression(paste(italic("Aedes"), " Bite Rate"))
  }
  
  
  if(colx == "vax"){
    x.lab.name = "Proportion Vaccinated"
    df1[[colx]] <-  unique(1-exp(-df1[[colx]]*365))
  }
  if(colx == "Tasl"){
    x.lab.name = expression(paste("Host-to-", italic("Aedes"), " Transmission Rate"))
  }
  
  #Y axis numbers and names
  if(coly == "Day_of_Extinction"){
    y.lab.name = "Years of Persistence"
    df1[[coly]] <- round(df1[[coly]]/365,0)
  }
  if(coly == "Median_Annual_Seroprevalence"){
    y.lab.name = "Median Seroprevalence"
  }
  if(coly == "Mean_Annual_Seroprevalence"){
  y.lab.name = "Mean Seroprevalence"
  }
  
  
  if(colx == "vax"){
    df1[[color1]] <- as.factor(df1[[color1]])

    Fig <- ggplot(df1, aes_string(x = "PropVax", y = coly, color = color1))+
      geom_line()+
      geom_point()+
      scale_colour_manual(name  = legend.name, breaks= discrete_vec, labels=discrete_vec2, values = color_vec)+
      labs(x = x.lab.name, y = y.lab.name)+
      thesis_theme
  }else{

  Fig <- ggplot(df1, aes_string(x = colx, y = coly, color = color1))+
    geom_line() +
    geom_point() +
    labs(x = x.lab.name, y = y.lab.name)+
    scale_colour_manual(name  = legend.name, breaks= discrete_vec, labels=discrete_vec2, values = color_vec)+
    thesis_theme
  }
  return(Fig)
}


###########################################################################
#2
#' plot.between.years <- function(df1, yr1, yr2){
#' 
#'   df1$Year = as.numeric(df1$Year)
#'   
#'   yr.list = seq(from = yr1, to = yr2, by = 1)
#'   
#'   filt = df1[(df1$Year %in% yr.list),]
#'   
#'   time1 = head(filt$time, 1)
#'   
#'   time2 = tail(filt$time, 1)
#'   
#' #_Aedes_ Mosquitoes
#' MosqAplot <-ggplot(filt, aes(time)) + 
#'   geom_line(aes(y = SA, colour = "SA")) + 
#'   geom_line(aes(y = IA, colour = "IA")) +
#'   #geom_line(aes(y = NAedes, colour = "NAedes")) +
#'   labs( x = "Time", y = "Aedes Population", color = "Legend Title\n") +      
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("SA" = "black","IA"= "red")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' print(MosqAplot)
#' 
#' Address <- "./Publication_Figures/Figures of partial simulations_Publication"
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_SAedes_Pop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=MosqAplot, dpi=72)
#' 
#' MosqIAEplot <- ggplot(filt, aes(time)) + 
#'   geom_line(aes(y = IAE, colour = "IAE")) + 
#'   labs(x = "Time", y = "Infected Aedes \nEggs Population") +
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("IAE" = "red")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' print(MosqIAEplot)
#' #' 
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_I_Aedes_Eggs_Pop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=MosqIAEplot, dpi=72)
#' 
#' 
#' MosqSAEplot <- ggplot(filt, aes(time)) + 
#'   geom_line(aes(y = SAE, colour = "SAE")) + 
#'   labs(x = "Time", y = "Susceptible Aedes \nEggs Population") +
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("SAE" = "black")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' print(MosqSAEplot)
#' #' 
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_S_Aedes_Eggs_Pop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=MosqSAEplot, dpi=72)
#' 
#' #' _Culex_ Mosquitoes
#' MosqCplot <- ggplot(filt, aes(time)) + 
#'   geom_line(aes(y = SC, colour = "SC")) + 
#'   geom_line(aes(y = IC, colour = "IC")) +
#'   labs(x = "Time", y = "Culex Population") +
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("SC" = "black","IC"= "red")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' print(MosqCplot)
#' 
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_Culex_Pop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=MosqCplot, dpi=72)
#' 
#' #' Total Infected Mosquitoes
#' MosqIAll <-ggplot(filt, aes(time)) + 
#'   geom_line(aes(y = IA, colour = "IA")) + 
#'   geom_line(aes(y = IC, colour = "IC")) +
#'   labs( x = "Time", y = "Infected Mosquito \nPopulation") + 
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("IA" = "red","IC"= "pink")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' 
#' print(MosqIAll)
#' 
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_All_infected_Mosq_Pop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=MosqIAll, dpi=72)
#' 
#' #'Sheep and Lambs
#' SLplot <-ggplot(filt, aes(x=time)) + 
#'   geom_line(aes(y = SS, colour = "SS")) + 
#'   geom_line(aes(y = IS, colour = "IS")) +
#'   geom_line(aes(y = RS, colour = "RS")) +
#'   geom_line(aes(y = VS, colour = "VS")) +
#'   geom_line(aes(y = SL, colour = "SL")) + 
#'   geom_line(aes(y = IL, colour = "IL")) +
#'   geom_line(aes(y = RL, colour = "RL")) +
#'   geom_line(aes(y = AL, colour = "AL")) +
#'   geom_line(aes(y = VL, colour = "VL")) +
#'   labs( x = "Year", y = "Number of Sheep and Lambs") + 
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("SS" = "black","IS"= "red", "RS" = "green", "VS" = "blue", "SL" = "gray","IL"= "pink", "RL" = "light green", "VL" = "light blue", "AL" = "violet")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' print(SLplot)
#' 
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_Sheep_SIR_Pop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=SLplot, dpi=72)
#' 
#' #Infected sheep and lambs
#' ISLplot <-ggplot(filt, aes(x=time)) + 
#'   geom_line(aes(y = IS, colour = "IS")) +
#'   geom_line(aes(y = IL, colour = "IL")) +
#'    labs( x = "Year", y = "Number of \nSheep and Lambs") + 
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("IS"= "red", "IL"= "pink")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' print(ISLplot)
#' 
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_Sheep_I_Pop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=ISLplot, dpi=72)
#' 
#' MosqSACplot <-ggplot(filt, aes(x=time)) + 
#'   geom_line(aes(y = SA, colour = "SA")) +
#'   geom_line(aes(y = SC, colour = "SC")) +
#'   labs( x = "Year", y = "Number of \nAedes and Culex") + 
#'   scale_x_continuous(name = "Year", breaks = seq(time1, time2, by = 365), labels = unique(filt$Year)) +
#'   scale_colour_manual("", values =c("SA"= "Black", "SC"= "purple")) +
#'   theme_classic() +
#'   theme(axis.text = element_text(size = 16, colour = "black")) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#'   theme(axis.title = element_text(size = rel(1.5))) + 
#'   theme(legend.text = element_text(size = 18)) + 
#'   theme(plot.title = element_text(size=22))
#' print(MosqSACplot)
#' 
#' FileName  <- paste0("Years_", yr1, "_", yr2, "_Mosq_S_TotPop_plot_Publication.png")
#' ggsave(here(read_model_run_path(),Address,  FileName), plot=MosqSACplot, dpi=72)
#' 
#' ggarrange(ISLplot, MosqAplot, MosqIAll, MosqSACplot, MosqIAEplot, MosqCplot, MosqSAEplot,   nrow = 4, ncol = 2)
#' 
#' }

element_custom <- function() {
  structure(list(), class = c("element_custom", "element_text"))
}
