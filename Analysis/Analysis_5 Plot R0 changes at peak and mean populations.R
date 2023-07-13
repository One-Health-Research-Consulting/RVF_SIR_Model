#' Title: R0 at mean population sizes
#' Author: Mindy Rostal
#' Date: 5/3/2022
#' 
#' 
#' Purpose: To add lines of the effect that varying the selected parameters have on the estimate of R0 at mean host and vector populations. 
#' This file can only be run after 3- Calculate and Plot_R0.R.

#Define new population set
mean_A_only <- c(NS = m_NS, NL = m_NL, Na = m_NAedes,  NC = 0) 
mean_C_only <- c(NS = m_NS, NL = m_NL, Na = 0,         NC = m_NC) 
################################################################################
#Prepare empty df and lists
emt_m <- list()
dat_m <- data.frame()

#Get a list of all the data frames. Each dataframe has the value of R0 for each population size as you vary the variables listed in R0_vec
R0_plot_dat_mean <-  lst.R0.var(empty.dat = dat_m, empty.lst = emt_m, both.pop = mean_popn, A.pop = mean_A_only, C.pop = mean_C_only, R0params.func = R0params, get.fun.1 = calc_R0, var_vec = R0_vec)

#Get the points where R0 is for the variable value that is used in my model
mean_pts = data.frame("R0_both"= calc_R0(R0params, mean_popn), #Calulate R0 for the three mosquito levels
                 "R0_A" = calc_R0(R0params, mean_A_only),
                 "R0_C" = calc_R0(R0params, mean_C_only),
                 "R0_both_peak"= calc_R0(R0params, peak_popn), #Calulate R0 for the three mosquito levels
                 "R0_A_peak" = calc_R0(R0params, peak_A_only),
                 "R0_C_peak" = calc_R0(R0params, peak_C_only))

#############################################################################
#Individual plots
#Tasl
df_plot <- as.data.frame(R0_plot_dat_mean[[1]])
x <- unique(df_plot$Variable)
mean_pts$X <- R0params[x]
if(!names(R0params[x])=="Tasl"){
  stop()
}
plot.Tasl_mean <- plot.Tasl+ 
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1)  + 
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1)  +
  #Original R0 points for legend
  geom_point(data = mean_pts, aes(x = X, y = R0_both_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts,aes(x = X, y = R0_A_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"), size = 2) +
  #Mean R0 points
  geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), size = 2) +
  geom_point(data = mean_pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  labs(x = expression(paste(italic("Aedes"), "-to-Host Transmission Rate")), y = expression(paste("R" [0]))) +
  plot_theme + 
  theme(legend.position="none")

#muA
df_plot <- as.data.frame(R0_plot_dat_mean[[2]])
x <- unique(df_plot$Variable)
mean_pts$X <- R0params[x]
if(!names(R0params[x])=="muA"){
  stop()
}
plot.muA_mean <- plot.muA+ 
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1)  +
  geom_point(data = mean_pts, aes(x = X, y = R0_both_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts,aes(x = X, y = R0_A_peak, col = "gray"),  size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"), size = 2) +
    #Mean R0 points
    geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), size = 2) +
    geom_point(data = mean_pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
    geom_point(data = mean_pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
    scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                       labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  labs(x = expression(paste(italic("Aedes"), " Mortality Rate")), y = expression(paste("R" [0]))) +
  plot_theme + 
  theme(legend.position="none")

#biteA
df_plot <- as.data.frame(R0_plot_dat_mean[[3]])
x <- unique(df_plot$Variable)
mean_pts$X <- R0params[x]
if(!names(R0params[x])=="biteA"){
  stop()
}
plot.biteA_mean  <- plot.biteA + 
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1)  +
  geom_point(data = mean_pts, aes(x = X, y = R0_both_peak, col = "gray"),  size = 2) +
  geom_point(data = mean_pts,aes(x = X, y = R0_A_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"),  size = 2) +
    #Mean R0 points
    geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), size = 2) +
    geom_point(data = mean_pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
    geom_point(data = mean_pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
    scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                       labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  labs(x = expression(paste(italic("Aedes"), " Bite Rate")), y = expression(paste("R" [0]))) +
  plot_theme + 
  theme(legend.position="none")

#q
df_plot <- as.data.frame(R0_plot_dat_mean[[4]])
x <- unique(df_plot$Variable)
mean_pts$X <- R0params[x]
if(!names(R0params[x])=="q"){
  stop()
}
plot.q_mean <- plot.q +
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1)  +
  geom_point(data = mean_pts, aes(x = X, y = R0_both_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_A_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"), size = 2) +
    #Mean R0 points
    geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), size = 2) +
    geom_point(data = mean_pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
    geom_point(data = mean_pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
    scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                       labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  labs(x = "Transovarial Transmission Rate", y = expression(paste("R" [0]))) +
  plot_theme + 
  theme(legend.position="none")

#Tcsl
df_plot <- as.data.frame(R0_plot_dat_mean[[5]])
x <- unique(df_plot$Variable)
mean_pts$X <- R0params[x]
if(!names(R0params[x])=="Tcsl"){
  stop()
}
plot.Tcsl_mean <- plot.Tcsl + 
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"))  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"))  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"))  +
  geom_point(data = mean_pts, aes(x = X, y = R0_both_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts,aes(x = X, y = R0_A_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"), size = 2) +
    #Mean R0 points
    geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), size = 2) +
    geom_point(data = mean_pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
    geom_point(data = mean_pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
    scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                       labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  labs(x = expression(paste(italic("Culex"), "-to-Host Transmission Rate")), y = expression(paste("R" [0]))) +
  plot_theme + 
  theme(legend.position="none")

#muC
df_plot <- as.data.frame(R0_plot_dat_mean[[6]])
x <- unique(df_plot$Variable)
mean_pts$X <- R0params[x]
if(!names(R0params[x])=="muC"){
  stop()
}
plot.muC_mean <- plot.muC +
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"))  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"))  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"))  +
  geom_point(data = mean_pts, aes(x = X, y = R0_both_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts,aes(x = X, y = R0_A_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"), size = 2) +
    #Mean R0 points
    geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), size = 2) +
    geom_point(data = mean_pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
    geom_point(data = mean_pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
    scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                       labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  labs(x = expression(paste(italic("Culex"), " Mortality Rate")), y = expression(paste("R" [0]))) +
  plot_theme  + 
  theme(legend.position="none")

#biteC
df_plot <- as.data.frame(R0_plot_dat_mean[[7]])
x <- unique(df_plot$Variable)
mean_pts$X <- R0params[x]
if(!names(R0params[x])=="biteC"){
  stop()
}
plot.biteC_mean <- plot.biteC +
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"))  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"))  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"))  +
  geom_point(data = mean_pts, aes(x = X, y = R0_both_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts,aes(x = X, y = R0_A_peak, col = "gray"), size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"), size = 2) +
    #Mean R0 points
    geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), size = 2) +
    geom_point(data = mean_pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
    geom_point(data = mean_pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
    scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                       labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  labs(x = expression(paste(italic("Culex"), " Bite Rate")), y = expression(paste("R" [0]))) +
  plot_theme + 
  theme(legend.position="none")

# Extract the legend as the 8th plot
leg <- ggplot() +
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), alpha=0)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), alpha=0)  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), alpha=0)  +
  geom_point(data = mean_pts, aes(x = X, y = R0_C_peak, col = "gray"), alpha=0 , size = 2) +
  geom_point(data = mean_pts, aes(x = X, y = R0_both, col = "coral4"), alpha=0 , size = 2) +
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3", "coral4" = "coral4", "gray" = "gray" ), 
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), paste(italic("Culex"), " Only"), paste("Mean Mosquito Populations"), paste("Mean Peak Mosquito Populations"))) +
  guides(color = guide_legend(override.aes = list(alpha=1, size = 5))) +
  theme_void()+
  theme(legend.position = c(0.65,0.5),
        legend.key.size = unit(.25, "cm"),
        legend.text.align = 0,
        legend.text = element_text(size =  7),
        legend.title = element_text(size = 8, face = "bold")) #+
        #guides(color = guide_legend(override.aes = list(linewidth=10)))
  #theme(legend.position = c(0.5,0.5),
  #      legend.key.size = unit(1, "cm"),
  #      legend.text.align = 0,
  #      legend.text = element_text(size =  12),
  #      legend.title = element_text(size = 15, face = "bold"))

leg

plot.list <- list(plot.Tasl_mean, plot.Tcsl_mean, 
                  plot.muA_mean, plot.muC_mean, 
                  plot.biteA_mean, plot.biteC_mean, 
                  plot.q_mean, leg)




FigS7.R0 <- ggarrange( plot.Tasl_mean, plot.Tcsl_mean, plot.muA_mean, plot.muC_mean, plot.biteA_mean, plot.biteC_mean, 
                      plot.q_mean, leg,
                      ncol = 2, nrow = 4, 
                      labels = c("A", "B", "C", "D", "E", "F", "G", ""))



ggexport(FigS7.R0, filename = "Publication_Figures/Fig S7 R0 change with params plots mean and mean peak pops.pdf", ncol = 2, nrow = 4, width = 5, height = 7)

#ggexport(FigS7.R0, filename = "Publication_Figures/Fig S7 R0 change with params plots mean and mean peak pops.png", ncol = 2, nrow = 4, width = 880, height = 880)
