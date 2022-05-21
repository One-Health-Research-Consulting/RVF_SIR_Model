### run scripts as jobs in the background

library(rstudioapi)

jobRunScript(path = "./1- Run RVFV Simulations.R",
             name = "RVFV Simulation")

jobRunScript(path = "./2_RVF_LHC_Sensitivity_Analysis.R",
             name = " LHC Sensitivity Analysis")

jobRunScript(path = "./3- Calculate and Plot_R0.R",
             name = "Calculate and plot R0")


