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

#Source Parameters
#source(here("", "1- Run RVFV Simulations.R")) ## ideall would save an .Rdata file and reload
source(here("Functions", "Function 4 Calculate R0.R"))
source(here("Functions", "Function 5 Calculate R0 dfs for plot.R"))
source(here("Functions", "Function 7 Manage model outputs.R"))

model_run_path <- read_model_run_path()

load_path <- sprintf("%s/rvfv_sim.Rdata",model_run_path)

# if the file doesnt exist, wait 10 mins and check again
wait_for_outputs(load_path = load_path)

load(load_path)

# Define popluation sizes for different R0 scenarios
mean_popn   <- c(NS = m_NS, NL = m_NL, Na = m_NAedes,       NC = m_NC) 
peak_popn   <- c(NS = m_NS, NL = m_NL, Na = m_peak_NAedes,  NC = m_peak_NC) 
max_popn    <- c(NS = m_NS, NL = m_NL, Na = max_NAedes,     NC = max_NC) 
peak_A_only <- c(NS = m_NS, NL = m_NL, Na = m_peak_NAedes,  NC = 0) 
peak_C_only <- c(NS = m_NS, NL = m_NL, Na = 0,              NC = m_peak_NC) 


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
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), 
                                         paste(italic("Culex"), " Only"))) +
  labs(x = expression(paste(italic("Aedes"), "-to-Host Transmission Rate")), y = expression(paste("R" [0]))) +
  thesis_theme +
  theme(legend.text.align = 0)

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
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), 
                                         paste(italic("Culex"), " Only"))) +
  labs(x = expression(paste(italic("Aedes"), " Mortality Rate")), y = expression(paste("R" [0]))) +
  thesis_theme+
  theme(legend.text.align = 0)

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
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), 
                                         paste(italic("Culex"), " Only"))) +
  labs(x = expression(paste(italic("Aedes"), " Bite Rate")), y = expression(paste("R" [0]))) +
  thesis_theme +
  theme(legend.text.align = 0)

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
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), 
                                         paste(italic("Culex"), " Only"))) +
  labs(x = "Transovarial Transmission Rate", y = expression(paste("R" [0]))) +
  thesis_theme +
  theme(legend.text.align = 0)

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
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), 
                                         paste(italic("Culex"), " Only"))) +
  labs(x = expression(paste(italic("Culex"), "-to-Host Transmission Rate")), y = expression(paste("R" [0]))) +
  thesis_theme +
  theme(legend.text.align = 0)

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
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), 
                                         paste(italic("Culex"), " Only"))) +
  labs(x = expression(paste(italic("Culex"), " Mortality Rate")), y = expression(paste("R" [0]))) +
  thesis_theme +
  theme(legend.text.align = 0)

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
                     labels = expression(paste(italic("Aedes"), " and ", italic("Culex")),  paste(italic("Aedes"), " Only"), 
                                         paste(italic("Culex"), " Only"))) +
  labs(x = expression(paste(italic("Culex"), " Bite Rate")), y = expression(paste("R" [0]))) +
  thesis_theme +
  theme(legend.text.align = 0)


plot.list <- list(plot.Tasl, plot.Tcsl, 
               plot.muA, plot.muC, 
               plot.biteA, plot.biteC, 
               plot.q)

FigS7.R0.init <- ggarrange( plot.Tasl, plot.Tcsl, plot.muA, plot.muC, plot.biteA, plot.biteC, 
                      plot.q,
                      ncol = 2, nrow = 4, 
                      labels = c("A", "B", "C", "D", "E", "F", "G"))




## create file name ----
figs7_path <- sprintf("%s/Publication_Figures/Draft_Figures/Fig S7_draft R0 change with params plots mean peak only.png", model_run_path)

ggexport(FigS7.R0.init, filename = figs7_path, 
         ncol = 2, nrow = 4, width = 800, height = 880)

#Plot Fig S7 which adds three lines of R0 at the mean populations to Fig S8
source(here("Analysis", "Analysis 5 Plot R0 changes at peak and mean populations.R"))

###############################################################
# Figure S8 Vary each parameter to see the affect on R0
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

  
  #x$x <- factor(x$x, levels=unique(x$x))
clim_vec <- unique(All.param.plot.dat$param[1:ind1])
sheep_vec <- unique(All.param.plot.dat$param[(ind1+1):ind2])#ind2-ind1])
Aedes_vec <- unique(All.param.plot.dat$param[(ind2+1):(ind3)])
Culex_vec <- unique(All.param.plot.dat$param[(ind3+1):(ind4)])

All.param.plot.dat <- All.param.plot.dat%>%
  mutate(TaxaGroup = if_else(param %in% clim_vec, "Climate", if_else(param %in% sheep_vec, "Host", 
                                                                     if_else(param %in% Aedes_vec, "Aedes", 
                                                                             if_else(param %in% Culex_vec, "Culex", "NA")))))%>%
  filter(!param == "vax")

#set factors
All.param.plot.dat$param <- factor(All.param.plot.dat$param, levels = rev(as.character(unique(All.param.plot.dat$param))))

#Plot Fig S8
All.param.plot <- ggplot(All.param.plot.dat) +
  geom_boxplot(aes(x = param, y = R0, colour = TaxaGroup)) + 
  scale_x_discrete(labels=expression(paste(italic("Culex"), " bite rate"), paste("Transmission ", italic("Culex"), "-to-ruminants"), 
                                     paste("Transmission ruminants-to-", italic("Culex")), paste(italic("Culex"), " carrying capacity"), 
                                     paste(italic("Culex"), " egg mortality rate"), paste("Adult ", italic("Culex"), " death rate"), 
                                     paste("Days before ", italic("Culex"), " first blood meal"), paste(italic("Culex"), " hatching rate"), 
                                     paste("Survival fraction of ", italic("Culex"), " larve/pupae"), 
                                     paste("Proportion of infected ", italic("Culex"), " eggs that survive"), paste("Number of ", italic("Culex"), " eggs laid"), 
                                     paste(italic("Culex"), " egg laying rate"), paste("Rate new ", italic("Aedes"), " eggs develop for hatching"), 
                                     paste("Transovarial transmission"), paste("Transmission ", italic("Aedes"), "-to-ruminants"), 
                                     paste("Transmission ruminants-to-", italic("Aedes")), paste(italic("Aedes"), " bite rate"), 
                                     paste("Extrinsic incubation"), paste("Days before ", italic("Aedes"), " first blood meal"), 
                                     paste(italic("Aedes"), " hatching rate"), paste("Survival fraction of ", italic("Aedes"), " larve/pupae"), 
                                     paste(italic("Aedes"), " carrying capacity"), paste(italic("Aedes"), " egg mortality rate"), 
                                     paste(italic("Aedes"), " mortality rate"), paste("Number of ", italic("Aedes"), " eggs laid"), 
                                     paste(italic("Aedes"), " egg laying rate"), paste("Host carrying capacity"), paste("Rate maternal antibodies wane"), 
                                     paste("RVFV abortion rate"), paste("Lamb specific RVF mortality"), paste("Lamb death rate"), 
                                     paste("Rate lamb are purchased at"), paste("Lamb birth rate"), paste("Sheep specific RVF mortality"), 
                                     paste("Rate sheep are sold"), paste("Sheep death rate"), paste("Lamb growth rate"), paste("Host recovery rate"), 
                                     paste("Days ", italic("Aedes"), " can hatch"), paste("Days after ", italic("Aedes Culex"), " can hatch"), 
                                     paste("Minimum rainfall for ", italic("Culex"), " to hatch"), paste("Minimum rainfall for ", italic("Aedes"), " to hatch"), 
                                     paste("Temperature threshold to hatch mosquitoes"), paste("Days of rainfall to hatch ", italic("Culex")), paste("Days of rainfall to hatch ", italic("Aedes")) ))+
  scale_color_manual(name = "Type of Parameter", breaks = unique(All.param.plot.dat$TaxaGroup), values = c("darkgreen", "orange",  "Indianred2", "darkslategray3" )) +
  coord_flip() + 
  labs(y = "Reproduction Number", x = "Parameter", color = "Type of Parameter")+
  thesis_theme

All.param.plot <- ggarrange(All.param.plot)

figS8_path<- sprintf("%s/Publication_Figures/Fig S8 R0 Senstivity to all Parameters.png",model_run_path)

ggexport(All.param.plot, filename = figS8_path, 
         width = 779, height = 597)

###############################################################
#Calculate the R0 values
R0_max <- round(calc_R0(R0params, max_popn), digits=3)
R0_peak <- round(calc_R0(R0params, peak_popn), digits=3)

cat("Mean popns:      R0 =", round(calc_R0(R0params, mean_popn), digits=3), "\n")
cat("Peak popns:      R0 =", round(calc_R0(R0params, peak_popn), digits=3), "\n")
cat("Max popns:       R0 =", round(calc_R0(R0params, max_popn), digits=3), "\n")
