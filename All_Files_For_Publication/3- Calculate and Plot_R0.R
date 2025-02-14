#' Title: Calculate and Plot R0
#' Author: Mindy Rostal,
#' Date: 5/3/2022
#' 
#' 
#' Purpose: To calculate R0 at different population sizes, produce Figures S7 and S8. Figure S7 is produced using the mean peak vector populations. 

library(here)
#Only need these libraries if you aren't sourcing the 1- Run RVFV Simulations.R file.
#library(stringr)
#library(ggplot2)
#library(ggtext) #If you haven't already run Analysis_1 Code for multiplots.R file

#Source Parameters
source(here("All_Files_For_Publication", "1- Run RVFV Simulations.R"))
source(here("All_Files_For_Publication/Functions", "Function 4 Calculate R0.R"))
source(here("All_Files_For_Publication/Functions", "Function 5 Calculate R0 dfs for plot.R"))

# Define popluation sizes for different R0 scenarios
mean_popn   <- c(NS = m_NS, NL = m_NL, Na = m_NAedes,       NC = m_NC) 
peak_popn   <- c(NS = m_NS, NL = m_NL, Na = m_peak_NAedes,  NC = m_peak_NC) 
max_popn    <- c(NS = m_NS, NL = m_NL, Na = max_NAedes,     NC = max_NC) 
peak_A_only <- c(NS = m_NS, NL = m_NL, Na = m_peak_NAedes,  NC = 0) 
peak_C_only <- c(NS = m_NS, NL = m_NL, Na = 0,              NC = m_peak_NC) 

#Set ggplot theme for printing PDFs (for pngs change to thesis_theme and adjust saving file type)
plot_theme <- theme_classic() +
  theme(plot.margin=unit(c(t=0.5,r = 0.5,b=0,l = 0.5),"cm"),
        axis.title = element_markdown(size = 13, family = "sans"),
        axis.text = element_markdown(size = 9, family = 'sans'),
        axis.text.x = element_markdown(angle = 90, hjust = 1),
        legend.title = element_markdown(size = 12, family = 'sans'),
        legend.text = element_markdown(size = 10, family = 'sans', margin = margin(r = .5, unit = 'cm')),
        legend.key.size = unit(.4, "cm"))

################################################################################
#Prepare empty df and lists
emt <- list()
dat <- data.frame()

#Give vector of variables that you want to plot - note if add or change order will need to change order of plots below and in file to plot supplemental figure at mean pops
R0_vec <- c("Tasl", "muA", "biteA", "q", "Tcsl", "muC", "biteC")

#Get a list of all the data frames. Each dataframe has the value of R0 for each population size as you vary the variables listed in R0_vec
R0_plot_dat <-  lst.R0.var(empty.dat = dat, empty.lst = emt, both.pop = peak_popn, A.pop = peak_A_only, C.pop = peak_C_only, R0params.func = R0params, get.fun.1 = calc_R0, var_vec = R0_vec)

#Get the points where R0 is for the variable values that are used in my model
#Calulate R0 for the three mosquito levels
pts = data.frame("R0_both"= calc_R0(R0params, peak_popn), 
                 "R0_A" = calc_R0(R0params, peak_A_only),
                 "R0_C" = calc_R0(R0params, peak_C_only))

#############################################################################
#Individual plots for Figure S7
#Tasl
df_plot <- as.data.frame(R0_plot_dat[[1]])
x <- unique(df_plot$Variable)
pts$X <- R0params[x]
if(!names(R0params[x])=="Tasl"){
  stop()
}
plot.Tasl <- ggplot()+#
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_point(data = pts, aes(x = X, y = R0_both), col = "coral4", size = 2) +
  geom_point(data = pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  ylim(0, 4)+
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3" ), 
                     labels = c("*Aedes* and *Culex*",  "*Aedes* Only", 
                                         "*Culex* Only")) +
  labs(x = "*Aedes*-to-Host Transmission Fraction", y = "R<sub>0</sub>") +
  plot_theme 

plot.Tasl

#muA
df_plot <- as.data.frame(R0_plot_dat[[2]])
x <- unique(df_plot$Variable)
pts$X <- R0params[x]
if(!names(R0params[x])=="muA"){
  stop()
}
plot.muA <- ggplot()+#
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_point(data = pts, aes(x = X, y = R0_both), col = "coral4", size = 2) +
  geom_point(data = pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  ylim(0, 4)+
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3" ), 
                     labels = c("*Aedes* and *Culex*",  "*Aedes* Only", 
                                         "*Culex* Only")) +
  labs(x = "*Aedes* Mortality Rate", y = "R<sub>0</sub>") +
  plot_theme

#biteA
df_plot <- as.data.frame(R0_plot_dat[[3]])
x <- unique(df_plot$Variable)
pts$X <- R0params[x]
if(!names(R0params[x])=="biteA"){
  stop()
}
plot.biteA <- ggplot()+#
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_point(data = pts, aes(x = X, y = R0_both), col = "coral4", size = 2) +
  geom_point(data = pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  ylim(0, 50)+
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3" ), 
                     labels = c("*Aedes* and *Culex*",  "*Aedes* Only", 
                     "*Culex* Only")) +
  labs(x = "*Aedes* Bite Rate", y = "R<sub>0</sub>") +
  plot_theme 

#q
df_plot <- as.data.frame(R0_plot_dat[[4]])
x <- unique(df_plot$Variable)
pts$X <- R0params[x]
if(!names(R0params[x])=="q"){
  stop()
}
plot.q <- ggplot()+#
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_point(data = pts, aes(x = X, y = R0_both), col = "coral4", size = 2) +
  geom_point(data = pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  ylim(0, 4)+
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3" ), 
                     labels = c("*Aedes* and *Culex*",  "*Aedes* Only", 
                                "*Culex* Only")) +
  labs(x = "Transovarial Transmission \nFraction", y = "R<sub>0</sub>") +
  plot_theme 

#Tcsl
df_plot <- as.data.frame(R0_plot_dat[[5]])
x <- unique(df_plot$Variable)
pts$X <- R0params[x]
if(!names(R0params[x])=="Tcsl"){
  stop()
}
plot.Tcsl <- ggplot()+#
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_point(data = pts, aes(x = X, y = R0_both), col = "coral4", size = 2) +
  geom_point(data = pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  ylim(0, 4)+
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3" ), 
                     labels = c("*Aedes* and *Culex*",  "*Aedes* Only", 
                                "*Culex* Only")) +
  labs(x = "*Culx*-to-Host Transmission \nFraction", y = "R<sub>0</sub>") +
  plot_theme 

#muC
df_plot <- as.data.frame(R0_plot_dat[[6]])
x <- unique(df_plot$Variable)
pts$X <- R0params[x]
if(!names(R0params[x])=="muC"){
  stop()
}
plot.muC <- ggplot()+#
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_point(data = pts, aes(x = X, y = R0_both), col = "coral4", size = 2) +
  geom_point(data = pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  ylim(0, 4)+
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3" ), 
                     labels = c("*Aedes* and *Culex*",  "*Aedes* Only", 
                                "*Culex* Only")) +
  labs(x = "*Culex* Mortality Rate", y = "R<sub>0</sub>") +
  plot_theme

#biteC
df_plot <- as.data.frame(R0_plot_dat[[7]])
x <- unique(df_plot$Variable)
pts$X <- R0params[x]
if(!names(R0params[x])=="biteC"){
  stop()
}
plot.biteC <- ggplot()+#
  geom_line(data = df_plot, aes(x = Value, y = R0_both, col = "1B"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_A, col = "2A"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_line(data = df_plot, aes(x = Value, y = R0_C, col = "3C"), size = 1, alpha = .5, linetype = "dashed")  +
  geom_point(data = pts, aes(x = X, y = R0_both), col = "coral4", size = 2) +
  geom_point(data = pts,aes(x = X, y = R0_A), col = "coral4", size = 2) +
  geom_point(data = pts, aes(x = X, y = R0_C), col = "coral4", size = 2) +
  geom_hline(aes(yintercept = 1.0), col = "dark grey", linetype = "dashed") +
  ylim(0, 50)+
  scale_color_manual(name = "Mosquito System", values = c("1B" = "slateblue2", "2A" = "Indianred2", "3C" ="darkslategray3" ), 
                     labels = c("*Aedes* and *Culex*",  "*Aedes* Only", 
                                "*Culex* Only")) +
  labs(x = "*Culex* Bite Rate", y = "R<sub>0</sub>") +
  plot_theme 


plot.list <- list(plot.Tasl, plot.Tcsl, 
               plot.muA, plot.muC, 
               plot.biteA, plot.biteC, 
               plot.q)

FigS4.R0.init <- ggarrange( plot.Tasl, plot.Tcsl, plot.muA, plot.muC, plot.biteA, plot.biteC, 
                      plot.q,
                      ncol = 2, nrow = 4, 
                      labels = c("A", "B", "C", "D", "E", "F", "G"))



ggexport(FigS4.R0.init, filename = "Publication_Figures/Fig S4_draft R0 change with params plots mean peak only.pdf", 
         ncol = 2, nrow = 4, width = 6, height = 8)

#Plot Fig S4 which adds three lines of R0 at the mean populations to Fig S4_draft
source(here("All_Files_For_Publication/Analysis", "Analysis_5 Plot R0 changes at peak and mean populations.R"))

###############################################################
# Figure S5 Vary each parameter to see the affect on R0
All.param.plot.dat <- data.frame(R0 = c(), param = c())
for (i in 1:length(R0params)){
  R0params_vary <- R0params
  label <- names(R0params_vary[i])
  param <- R0params_vary[i]
  test_vec <- param * c(0.5, 0.75, 1, 1.25, 1.5)
  for (j in 1:5){
    R0params_vary[i] <- test_vec[j]
    R0 <- calc_R0(R0params_vary, peak_popn)
    All.param.plot.dat <- rbind(All.param.plot.dat, data.frame(R0 = R0, param = label))
  }
}

#Add grouping by type of variable
ind1 <- tail(which(All.param.plot.dat$param == "DaysCanHatchA"),1)
ind2 <- tail(which(All.param.plot.dat$param == "NLmax"),1)
ind3 <- tail(which(All.param.plot.dat$param == "alphaAE"),1)
ind4 <- tail(which(All.param.plot.dat$param == "biteC"),1)

  

clim_vec <- unique(All.param.plot.dat$param[1:ind1])
sheep_vec <- unique(All.param.plot.dat$param[(ind1+1):ind2])
Aedes_vec <- unique(All.param.plot.dat$param[(ind2+1):(ind3)])
Culex_vec <- unique(All.param.plot.dat$param[(ind3+1):(ind4)])

All.param.plot.dat <- All.param.plot.dat%>%
  mutate(TaxaGroup = if_else(param %in% clim_vec, "Climate", if_else(param %in% sheep_vec, "Host", 
                                                                     if_else(param %in% Aedes_vec, "Aedes", 
                                                                             if_else(param %in% Culex_vec, "Culex", "NA")))))%>%
  filter(!param == "vax")

#set factors
All.param.plot.dat$param <- factor(All.param.plot.dat$param, levels = rev(as.character(unique(All.param.plot.dat$param))))

#Plot Fig S5
All.param.plot <- ggplot(All.param.plot.dat) +
  geom_boxplot(aes(x = param, y = R0, colour = TaxaGroup)) + 
  scale_x_discrete(labels=c("*Culex* bite rate", "*Culex*-to-ruminants Transmission", 
                                     "Ruminants-to-*Culex* Transmission", "*Culex* carrying capacity", 
                                     "Culex egg mortality rate", "Adult *Culex* mortality rate", 
                                     "Days before *Culex* first blood meal", "*Culex* hatching rate", 
                                     "Survival fraction of *Culex* larve/pupae", 
                                     "Proportion of infected *Culex* eggs that survive", "Number of *Culex* eggs laid", 
                                     "*Culex* egg laying rate", "Rate of new *Aedes* eggs development", 
                                     "Transovarial transmission", "*Aedes*-to-ruminants Transmission", 
                                     "Ruminants-to-*Aedes* Transmission", "*Aedes* bite rate", 
                                     "Extrinsic incubation", "Days before *Aedes* first blood meal", 
                                     "*Aedes* hatching rate", "Survival fraction of *Aedes* larve/pupae", 
                                     "*Aedes* carrying capacity", "*Aedes* egg mortality rate", 
                                     "*Aedes* mortality rate", "Number of *Aedes* eggs laid", 
                                     "*Aedes* egg laying rate", "Host carrying capacity", "Rate maternal antibodies wane", 
                                     "RVFV abortion rate", "Lamb specific RVF mortality", "Lamb mortality rate", 
                                     "Rate lamb are purchased at", "Lamb birth rate", "Sheep specific RVF mortality", 
                                     "Rate sheep are sold", "Sheep mortality rate", "Lamb growth rate", "Host recovery rate", 
                                     "Days *Aedes* can hatch", "Days after *Aedes*/*Culex* can hatch", 
                                     "Minimum rainfall for *Culex* to hatch", "Minimum rainfall for *Aedes* to hatch", 
                                     "Temperature threshold to hatch mosquitoes", "Days of rainfall to hatch *Culex*", "Days of rainfall to hatch *Aedes*"))+
    scale_color_manual(name = "Type of \nParameter", breaks = unique(All.param.plot.dat$TaxaGroup), values = c("darkgreen", "orange",  "Indianred2", "darkslategray3" )) +
  coord_flip() + 
  labs(y = "Reproduction Number", x = "Parameter", color = "Type of Parameter")+
  plot_theme

All.param.plot <- ggarrange(All.param.plot)

ggexport(All.param.plot, filename = "Publication_Figures/Fig S5 R0 Senstivity to all Parameters.pdf", 
         width = 7, height = 7)


###############################################################
#Calculate the R0 values
R0_max <- round(calc_R0(R0params, max_popn), digits=3)
R0_peak <- round(calc_R0(R0params, peak_popn), digits=3)

cat("Mean popns:      R0 =", round(calc_R0(R0params, mean_popn), digits=3), "\n")
cat("Peak popns:      R0 =", round(calc_R0(R0params, peak_popn), digits=3), "\n")
cat("Max popns:       R0 =", round(calc_R0(R0params, max_popn), digits=3), "\n")
