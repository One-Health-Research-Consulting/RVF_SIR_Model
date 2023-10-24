# RVF_SIR_Model
Repository of code to simulate 34-years of RVFV transmission at a single pan in central South Africa.

This project uses renv for dependency management.   
1. install the `renv` [package](https://rstudio.github.io/renv/index.html)
2. run `renv::activate()`
3. run `renv::restore()` to install the right versions of packages (this may take a while)

To run the simulations, run the scripts in order according to the file number. If the file name starts with the term Function or Model, then it should not be run by the user and will be called by the main scripts. If a file name starts with the term Analysis, then it may be run by the user depending on the level of analysis they would like to do. The main scripts will allow the user to run the 34-year simulation with the option of various scenarios (e.g., vaccination), run the Latin hypercube sensitivity analysis, run the variable-by-variable analyses and estimate R_0. The analysis scripts will produce plots and analyze the Latin hypercube results. The model scripts contain the parameter list, the mosquito hatching determination and the ODE function. The function scripts have all other functions defined for the analyses. 

1. Run the simulation:
Start with the “1-Run RVFV Simulations.R” file. The user will start by indicating whether they would like to run a specific scenario (with vaccination, without transovarial transmission, without horizontal transmission, a low starting population of infected Aedes eggs, and with a higher or lower Culex mortality rate). If the vaccination scenario is selected the user must also indicate the proportion of animals that will be vaccinated and may select scenarios with a 3 times higher or 25% lower vaccine proportion from what was selected. The user may also set vax.burst to true and then can choose from various years during which animals are vaccinated at the given vax.proportion over 7 days in July for each year that is indicated by the user.  To run these additional vaccine scenarios both “Vaccinate”, and the proportion higher or lower or vax.burst must be set to “TRUE”. If all scenarios are set to FALSE, then the full, exemplar simulation will be run. Note that the SA scenario should always be FALSE, as there is a separate code to run the Latin hypercube sensitivity analysis.

Source all required code. If a scenario is selected, then the code will adjust the parameter list as required.

The simulation is run, and results are converted to a data frame.

The Define.MosqYr adds the annual season (allowing the year to be assessed from September-August according to the mosquito season (MosqYr) rather than the calendar year).

Three plots are made for a quick spot check. SLplot shows the host dynamics, MosqIAEplot plots the population of infected Aedes eggs, MosqIAll plots the infected Aedes and Culex adults and PeakIMosq_Plot plots the ratio of infected Culex to Aedes vs infected livestock. A plot of the effective R0 is also included. For the vax.burst scenarios, the results for most plots are written as a csv file to faciliate combining of these plots for the publication. To make the plots from the publication, run the multiplot code (“Analysis 1 Code for multiplots.R”).

The code also calculates some metrics and provides a data frame of the number of infected hosts per season. Metrics include size of annual outbreaks, summarize the number of infected Culex and Aedes at the end of the year, calculate the mean larval/pupal development and mortality rates (which varies daily dependent on temperature), calculate the mean population sizes (for mosquitoes this only includes days when there is at least one adult mosquito present) and the mean mosquito population during each season’s peak for each species.

The code produces Table S1, which includes the mean seroprevalence, the mean annual maximum host-vector ratio, mean annual proportion of infected Aedes eggs, mean annual proportion of infected adult Aedes, and the mean annual proportion of infected adult Culex. The table will be saved if no other scenarios are selected (e.g., all scenarios are set to FALSE).

Calculate the effective R0 for each timestep of the simulation.

Files sourced:
*Function 1 Define Functions for Output of Sensitivity Analysis.R
*Model 3 RVFV ODE SIRS function.R
*Function 3 Plot simulations for a given time period.R
*Model 2 Mosquito hatch rates at daily timestep.R (which includes)
	**Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R
 	**Model 1 - RVFV Optimized Parameters for publication.R (sourced within Model 2 *Mosquito hatch rates at daily timestep.R)
*Function 4 Calculate R0.R
*Function 5 Calculate R0 dfs for plot.R

2. Run the Latin hypercube sensitivity analysis:
2_RVF_LHC_Sensitivity_Analysis.R. 
Ensure that SA is always set to TRUE (this ensures that the parameter ranges for the sensitivity analysis in the parameters code file is run). Set h as the number of simulations to run. It is automatically set to 4000, which is the number used in the publication. Please note that with this large number of simulations, the analysis takes a significant amount of computing power and time. Source all functions and climate data.

The Latin hypercube function was designed to run in parallel on multiple cores. The code was set to either run on 20 cores or if there are more cores than number of simulations to run, then use one core per simulation to maximize efficiency.

The hypercube parameters are inputted as a list and split by row. Two blank data frames need to be entered into the function so that they can be used in the function.

The results from the function are turned into a list and bound by rows, then the columns are named for each output. A file name is created using the current date and the data is saved.

Files sourced: 
*Function 1 Define Functions for Output of Sensitivity Analysis.R
*Function 6 Latin Hypercube Analysis.R
*Model 1 - RVFV Optimized Parameters for publication.R
*Model 3 RVFV ODE SIRS function.R
*Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R

3. Run the R0 analysis
Run the 3- Calculate and Plot_R0.R code. This code will calculate R_0 for various populations and then plot the sensitivity analyses. Use the function lst.R0.var (defined in Function 5 Calculate R0 dfs for plot.R) to produce a list of data frames, where each data frame has the value of R_0for each population size as you vary the variables listed in R0_vec. Calculate the value of R_0 using the values of the parameters used in the full simulation for the peak_popn, peak_A_only and peak_C_only and store in a dataframe.

The population vectors are defined for the hosts and vectors as follows:
mean_popn: All population means calculated across the entire simulation – the mean population of adult sheep, lambs and the mean population (when at least one adult mosquito of the species is present) of Aedes and Culex. 
peak_popn: The mean population size of the sheep and lambs calculated across the entire simulation. For the vectors, the maximum population size during each year of the simulation (peak) was averaged across the 34 years of the simulation. 
max_popn: The mean population size of the sheep and lambs calculated across the entire simulation. For the vectors, the maximum population size over the full simulation was used.
peak_A_only: The mean population size of the sheep and lambs calculated across the entire simulation. For the Aedes, the maximum population size during each year of the simulation (peak) was averaged across the 34 years of the simulation. No Culex are included.
peak_C_only: The mean population size of the sheep and lambs calculated across the entire simulation. For the Culex, the maximum population size during each year of the simulation (peak) was averaged across the 34 years of the simulation. No Aedes are included.
no_mosq: The mean population size of the sheep and lambs calculated across the entire simulation. No Aedes or Culex are included.

Produce the plots (Fig S7) showing how R_0 varies as you change a given parameter using the following vector populations: at the peak population, the peak population of Aedes only and the peak population of Culex only. The parameters plotted, in order are Tsal, muA, biteA, q, Tcsl, muC, and biteC. The parameters need to be plotted in the order of the R0_vec. This produces the initial plots using the mean peak vector populations.

Run the Analysis 5 Plot R0 changes at peak and mean populations.R file to add the lines for the mean population levels and complete Figure S7.

Plot Figure S8 in which we vary each parameter individually to be the value in the model times a factor (0.5, 0.75, 1, 1.25, 1.5) and calculate R0. Then create a boxplot for the range of R0 for each parameter.

Finally, calculate the R0 estimate at different populations.

Files sourced:
1- Run RVFV Simulations.R
Function 4 Calculate R0.R
Function 5 Calculate R0 dfs for plot.
Analysis 5 Plot R0 changes at peak and mean populations.R


Analysis Code
A1. Analyze the simulation results:
“Analysis/Analysis 1 Code for multiplots.R” 
Create plots of the simulation and scenarios. Run the code. This code will produce Figures 1-3; S2-S7 and S9-S12.

If the plots should be saved (and the plots for Figure 1 combined) set Multiplot to be TRUE. Set the plot theme elements to be used for all plots.

Make plots of the Aedes mosquito, Culex mosquito, infected Aedes egg, and all infected mosquito populations as well as of the host population. The outbreaks reported during the simulation time period in Pienaar et al (2013) are listed and those that were in the highveld area (Free State/Northern Cape Provinces) or were reported but from an unknown location are identified and included on the host plot. A plot of the effective R_0  on each timestep of the simulation is also produced. 

If no scenarios are selected and Multiplot is TRUE, the code will combine the population plots and the R_0 plot to make Figure 1. Figure S3 (infected hosts vs the ratio of infected Culex to Aedes) and S9b (the simulation run without changing the Culex mortality rate) are also made. The complied figure of S9 is produced using previously produced Figures of S9a and S9c.

If the scenario with the 25% lower Culex mortality rate is TRUE (muC.25_lower = TRUE) then Figure S9a is made). If the scenario with the 25% higher Culex mortality rate is TRUE (muC.25_higher = TRUE) then Figure S9c is made). 

Figure 2 is made next (assuming no scenarios are selected) using data from the R_0 analyses, looking at transovarial transmission vs Aedes bite rate and the impact on R_0 and seroprevalence. Similarly, Figure S10 is made looking at the same outcomes as transovarial transmission and Culex mortality rate changes.

Figure S5 is made using data from the sensitivity analysis where we compare changes in simulated seroprevalence and persistence as a single (S5a and S5b) variable is varied or as two variables are varied (S5c and S5d).

Figure 3 and the vaccination scenarios are plotted next assuming Vaccinate is set to TRUE. Using the host and infected Aedes egg populations plotted above and a plot of changes in persistence as transovarial transmission and vaccination fractions are varied. Figure S11b is also saved.

Figure S11c and S11a are then plotted if both Vaccinate is set to TRUE and vax.25_higher or vax.25_lower are set to TRUE, respectively. The plots are arranged and saved.

Figure S11 is compiled with the previously saved plots when Vaccinate is set to TRUE and no scenarios of 25% higher or lower are selected.

Figure S4 is produced if the scenario with no transovarial transmission (S4A) and no horizontal transmission (S4B) are selected. They are compiled when the no horizontal transmission scenario is selected.

Figures S2 (rainfall and temperature) and S6 (an enlarged segment of the effective R_0 plotis produced if no scenarios are selected.

Figure S12 is produced by selecting a random year of simulation and plotting the total Aedes and Culex populations, provided no simulations are selected. Finally, calculate the R0 estimate for the mean_popn, peak_popn and max_popn.

Files sourced:
*Function 3 Plot simulations of a given time period.R

A2. Examine how the simulated seroprevalence and persistence change as the values of select parameter pairs are varied. 
Run Analysis_2_Plot_seroprev_as_key_params_change. Note, depending on how many values are being evaluated for each parameter this can take significant computing power and time to run. The code can be run for the four variable pairs and corresponding outcomes described in the paper. Set one of the four to TRUE and keep the other three FALSE:

Q_biteA_Seroprev.mean: Transovarial Transmission vs Aedes bite rate evaluating seroprevalence
Q_muC_Seroprev.mean: Transovarial Transmission vs Culex mortality rate evaluating seroprevalence
Vax_Q_Persistence: Transovarial Transmission vs vaccination rate evaluating persistence
Q_Tasl_Persistence: Transovarial Transmission vs host-to-Aedes transmission rate evaluating persistence

Once a variable pair is selected, R0_plot may also be set to TRUE. If set to true it will evaluate the following outcomes: Mean seroprevalence, persistence (the timestep after which there are no more infected hosts) and R0 (calculated at the mean host and vector populations).

The values for each variable that were evaluated for the manuscript are given next. Then variables, functions and outcomes are defined for each specific variable pair. A blank data frame (summary.dat) is created and a counter parameter (j) is set. 

A for-loop runs through every combination of parameter pairs. For the variable pair that includes vaccination the initial populations are adjusted such that the designated proportion of the total sheep and lambs are vaccinated, and these are removed from the susceptible population.

The model is then run, and the output database is modified.

If the R0_plot is selected, then estimate the mean population sizes and development rate and calculate R0.

Add everything to the summary.dat data frame.

If the R0_plot is selected, and after all of the simulations in the for loop are completed, add a column with the value of R0 calculated when the transovarial transmission fraction is set to zero by filtering and merger the data frame. Calculate the seasonal and long-term persistence.

Name and save the .Rdata and .csv files.

Produce plots (also produced in the Analysis 1 Multiplot file)

Files sourced:
*Function 1 Define Functions for Output of Sensitivity Analysis.R
*Model 2 Mosquito hatch rates at daily timestep.R
*Model 3 RVFV ODE SIRS function.R
*Function 3 Calculate R0.R

A3. Analyze the effects of varying select parameters on the simulated seroprevalence and persistence. 

Run the Analysis_3_Plot_outcome_vs_single_key_param.R file. It is designed to vary one of four parameters: transovarial transmission, host-to-Aedes transmission, Aedes-to-host transmission and extrinsic incubation period. It can run one parameter at a time, which is set to TRUE at the beginning of the code. For each parameter, a simulation will be run for every value between 0 and 1 at 0.5 intervals and for the value used in the full model. After each simulation the mean seroprevalence and days of persistence are estimated. The data are then saved and single parameter plots against seroprevalence and persistence are produced and saved.

Files sourced:
*Function 1 Define Functions for Output of Sensitivity Analysis.R
*Model 3 RVFV ODE SIRS function.R
*Function 3 Plot simulations of a given time period.R
*Model 2 Mosquito hatch rates at daily timestep.R

A4. Analyze the data from the sensitivity analysis:
Run the Analysis 4 Assessing the sensitivity analysis.R file. This file will produce Table 1 from the output of the Latin hypercube sensitivity analysis (2_RVF_LHC_Sensitivity_Analysis.R). We also calculated the proportion of simulations that: become extinct, have a high seroprevalence, or persist and are within our target seroprevalence. 

To make the table, vectors are made of the parameters to compare and the outcomes. The data is subset to just the parameters being evaluated. Estimate the partial correlation coefficient for each variable. Put everything in a table with the estimate and the combined confidence interval. Save the table.

A5. Add lines of the effect that varying the selected parameters have on the estimate of R0 at mean host and vector populations

Run the Analysis 5 Plot R0 changes at peak and mean populations.R file (note: this file can only be run after 3- Calculate and Plot_R0.R). First make a list of data frames that gives the value of R0 for the three population sizes (mean_popn [mean host and vector populations], mean_A_only [mean host and Aedes populations] and mean_C_only [mean host and Culex populations]. Then make a vector of the R0 value of the parameters used in the full simulation. 

Make a plot of the R0 using mean and mean peak vector populations as each parameter is varied (Tsal, muA, biteA, q, Tcsl, muC, and biteC.). Combine each all of the plots and save the final figure.

Model_Scripts
M1. Optimized parameters used in the simulation.

File: Model 1 - RVFV Optimized Parameters.R

This file is sourced within the Model 2 *Mosquito hatch rates at daily timestep.R file. All model parameters are defined and placed into a vector for the full simulation, the R0 analysis and the sensitivity analysis. Full definitions and references are provided in Table S4.

M2. Define mosquito hatching days and daily rates.

File: Model 2 Mosquito hatch rates at daily timestep.R

This file pulls the climate data and uses functions (see below) to identify days in which the Culex and Aedes will hatch. Then it uses the approxfun() function to be able to feed the daily hatching data directly into the differential equation. 

Files sourced:
*Data: Combined Temp and Precip Data for RVF Simulation.csv
*Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R
*Model 1 - RVFV Optimized Parameters.R

M3. ODE function with full set of differential equations to define model.

File: Model 3 RVFV ODE SIRS function.R

This file describes a function that takes a series of timesteps, a population vector, a parameter vector and vectors to allow daily control over hatching and development rates. It describes the differential equations used to run the simulation (also described in the supplemental text Equations S4-S33) and which derivatives and populations to return.

Function Codes: 
*Function 1 Define Functions for Output of Sensitivity Analysis.R

A set of functions written to assess the outputs of the sensitivity analysis as well as other useful calculations. The following functions are defined:

Define.MosqYr() is a function that adds the seasonal (“MosqYear”/Month) info to the population data frame. The MosqYear is defined as being from September to August (e.g., September 1982-August 1983 = MosqYear 1982). It takes the following arguments: the simulation population data frame (df) and the precipitation/temperature data frame (precip.df). 

Get.Outbreak.Dataframe() is a function that makes a data frame of all outbreaks by year/season. It takes the following arguments: the simulation population data frame (df) and the sheep/lamb recovery rate (sigma_sl).

InfectedMosqPeak() is a function that produces a data frame indicating when and the magnitude of the largest population of vectors occurs each year. It takes the following argument: the simulation population data frame (df). The data frame has one row per simulation season (MosqYear). For the populations of infected Aedes, infected Culex, Susceptible Aedes and Susceptible Culex it indicates the timestep (“SimDay”) and the size of the maximum population on a single timestep during that season. It indicates which species has a higher peak of infected mosquitoes that season and the size of the maximum population of infected Aedes eggs that season. It calculates the ratio of the peak Culex and Aedes population for that season (note these will not have occurred on the same day). Finally, it binds the outbreak data frame to the peak infected mosquito data frame indicating the number of hosts infected during that season.   

Get.Mean.Vec.Pops is a function that calculates the mean of the vector populations (across timesteps where there is at least one adult mosquito of that species present). It takes the following arguments: the simulation population data frame (df) and the population parameter of interest (e.g., “NAedes”). It outputs three estimates: the overall mean population size (m_mosq_pop), the maximum peak population, which is the maximum seasonal population size (per “MosqYear”) of the simulation (max_mosq_pops), and the mean peak population, which is the mean of the seasonal vector populations during a 34-season simulation (m_peak_Mosq_pop). 

Get.Mean.SeroP.MosqY is a function that first identifies the largest proportion of recovered hosts (the seroprevalence) during that season (MosqYear), then calculates the mean of the seasonal proportions during the 34-year simulation. It takes the following arguments: the simulation population data frame (df).

Get.Extinct.Day is a function that identifies the timestep after which no hosts are infected (IS and IL are both. <1). Extinction in the host is our conservative definition for RVFV extinction. Thus, this function identifies how long the virus persists in the host populations. It takes the following arguments: the simulation population data frame (df).

Get.Max.Outbreak.Size.MosqYear is a function that sums the total number of infected hosts per season (MosqYear). During the calculation it is assumed for any host population, that if the number of infected hosts (IS and/or IL) is < 1, there are effectively 0 infected hosts in that population during that timestep. It then sums the number of infected animals for each season (divided by the recovery rate as an individual will stay infected for more than one timestep. The function outputs the number of infected hosts during the largest seasonal outbreak of the simulation. It takes the following arguments: the simulation population data frame (df) and the recovery rate (sigma).

Get.Mean.Outbreak.Size.MosqYear is a function that sums the total number of infected hosts per season, as described for the Get.Max.Outbreak.Size.MosqYear() function, then takes the mean across all of the seasons of the simulation (34) to provide the average size of seasonal outbreak.
It takes the following arguments: the simulation population data frame (df) and the recovery rate (sigma).

Get.Mean.Outbreak.lengths.MosqYear() is a function that sums the number of days an infected host is present during the season. Then it takes the mean across the seasons of the simulation to provide the average outbreak length per season. It takes the following argument: the simulation population data frame (df).

Get.Prop.Infected.Eggs() is a function that estimates the proportion of infected eggs on the last timestep of the 34-year simulation. It also estimates the range and mean of the proportion if infected eggs across the full simulation period. It takes the following argument: the simulation population data frame (df). It outputs four elements: mean.prop.i (the mean proportion of infected eggs over all timesteps), max.prop.i (the maximum proportion of infected eggs across all timesteps), min.prop.i (the minimum proportion of infected eggs across all timesteps), end.prop.i (the proportion of infected eggs at the final timestep of the simulation).

Get.Prop.Infected.Mosq() is a function that estimates the proportions of the infected population of any vector population (e.g., IA, IC, IAE). It takes the following arguments: the simulation population data frame (df) and the name of the column with the infected vector population of interest (i_pop; e.g. “IAE”). It calculates the proportions for timesteps when at least one adult mosquito of the given vector population is present and for the entire 34-year simulation. It outputs three elements: mean.prop (the mean proportion of infected eggs over all timesteps), range.prop.low (the minimum proportion of infected eggs across all timesteps), range.prop.hi (the maximum proportion of infected eggs across all timesteps).


F2. Define Aedes and Culex hatching days.

File: Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R

This file contains 5 functions to define the hatching days for the vector populations and to calculate the development rates for the Aedes and Culex larval/pupal stages (DayDefinition, AedesForcingEndemicMeanT, CulexForcingMeanT, dev_CLP, dev_ALP). The first three functions must be run on the climate data frame in order (DayDefinition, then AedesForcingEndemicMeanT, and finally CulexForcingMeanT).

DayDefinition: takes the following parameters: df (a climate data frame), rollcumrainA15 (number of days to include in estimate of the cumulative rainfall for Aedes hatching), cumrainC15 (number of days to include in estimate of the cumulative rainfall for Culex hatching), minrainC (the minimum amount of precipitation for Culex hatching), minrainA (the minimum amount of precipitation for Culex hatching). To help account for variability in hatching in Culex, which is based on standing water and for which we do not have data, we adjusted the proportion of eggs that hatch is given by taking the 3-day cumulative and dividing it by the maximum 3-cumulative precipitation that season. This allows more hatching when there is more precipitation. After running this function, each timestep is defined in terms of whether it surpasses the thresholds set by temperature and precipitation for hatching for Aedes and Culex and what proportion of Culex eggs may hatch.

AedesForcingEndemicMeanT: takes the following parameters: df (a climate data frame),  hatchabledayA (Eggs are only viable once a year for 14 days with precipitation in the season). The function adds a column to the data frame to dictate when Aedes egg hatching is permitted (on the first 14 days of the season that surpass both the temperature and precipitation thresholds).

CulexForcingMeanT: takes the following parameters: df (a climate data frame), dayspostAhatch (the number of days after the Aedes start hatching that the Culex can begin hatching). The function adds a column to the data frame to dictate when Culex egg hatching is permitted (on days of the season that surpass both the temperature and precipitation thresholds that occur after the Aedes are finished hatching).

dev_CLP: takes the following parameters: dat (a climate data frame), and species-specific parameters defined in Rueda et al 1980 (rh025, HA, TH, HH) to calculate the development of Culex larva/pupae. The calculation is found in Rueda et al 1980.

dev_ALP: takes the following parameters: dat (a climate data frame), and species-specific parameters defined in Rueda et al 1980 (rh025, HA, TH, HH) to calculate the development of Aedes larva/pupae. The calculation is found in Rueda et al 1980.

F3. Functions to support plotting.

File: Function 3 Plot simulations for a given time period.R

This file describes two functions: ParamByParamPlot and plot.between.years. 

ParamByParamPlot takes the following parameters df1 (data frame with the results of the parameter by parameter simulation [Analysis_2_Plot_seroprev_as_key_params_change.R]), colx (the parameter to be plotted on the X-axis), coly (the outcome factor to be plotted on the Y-axis), color1 (the second parameter being evaluated and presented as discreet data in the plot), no.leg (TRUE or FALSE, whether a legend should be plotted or not). This function is used to plot the parameter-by-parameter analyses included in Figures 3 and S5. 

plot.between.years takes the following parameters: df1 (the simulation data frame), yr1 (the lower bound of year range), yr2 (the upper bound of year range). This function is used to plot the simulation results between a specific subset of years (e.g., Figure S6 plots the simulation results between 2004-2006; also used in Figure S12).

F4. Functions to calculate R0 and effective R0

File: Function 4 Calculate R0.R

This file has two functions: calc_R0 and R_eff. 

calc_R0 takes the following parameters: parameters (a vector of parameters used in the simulation [from Model 1 - RVFV Optimized Parameters.R]), pop_size (vector of the host, Aedes and Culex populations). The R0 analysis was done in wxMaxima, which allowed a numeric solution to estimated. The algebraic equations and the transmission and transition matrices were defined by the wxMaxima analysis, then input into this function to allow the quick calculation of R0 when varying the parameters and the population sizes in the R ecosystem. 

First the larval/pupal mortality rates are calculated (from the mean development rates) and the transmission terms are calculated and combined. The proportions of susceptible sheep, lambs Aedes and Culex are estimated using equations produced by the wxMaxima analysis. Likewise, the adjusted birth rate for the system is calculated (using equations produced by the wxMaxima analysis). 

Next generation matrices were defined for a small domain K_S (using only the rows and columns of K_L belonging to new states-at-infection) to calculate R_0. We calculated the Jacobian matrix and decomposed it into the T matrix (the transmission parameters that describes the production of new infections) and Σ matrix (the development part that describes changes in state). The T and Σ matrices were transcribed from the wxMaxima analysis into the R analysis.

R_eff takes the following parameters: pop_matrix (a of populations, including those of the susceptible populations of hosts, Aedes and Culex populations and the MosqYear – must be in order SS, SL, SA, SC), params (a vector of parameters used in the simulation [from Model 1 - RVFV Optimized Parameters.R]) and get.fun1 (the R0 calculation function to run [calc_R0]). The function calculates the R0 for that population of susceptible hosts and vectors.

F5. Create data frames for the sensitivity analysis of R0 for RVFV.

File: Function 5 Calculate R0 dfs for plot.R

This file has one function in it: lst.R0.var. The function takes the following parameters: empty.dat (an empty database), empty.lst (an empty list), the host/vector populations to be evaluated - both.pop, A.pop, C.pop (indicate which vector of populations for the hosts, Aedes and Culex populations (both.pop), hosts and Aedes (A.pop) and hosts and Culex (C.pop) [e.g. mean_popn vs peak_popn]), R0params.func (the optimized parameters used in the R0 calculation (from Model 1 - RVFV Optimized Parameters.R), get.fun.1 (the function to run, e.g. calc_R0 from Function 4 Calculate R0.R) and var_vec (the list of variables that will be evaluated). The function will produce a list of data frames, where each data frame has the value of R0 for each population size as you vary the variables listed in var_vec. The calculated data frame is used to make the plots for Figure S7.

F6. Conduct the Latin Hypercube analysis

File: Function 6 Latin Hypercube Analysis.R

The function, HypercubeSA, with 21 inputs is described. The main simulation code from 1-Run RVFV Simulations.R and Model 2 Mosquito hatch rates at daily timestep.R included in the function. After the simulation is run, six outcome functions are run. They are detailed when the function is called, but must be left in order as some require specific arguments and these are hard coded into the function. Further, they need to be in order so that the column names applied after the function is run labels the results correctly. If needed an option to save each simulation data file can be uncommented and used to evaluate the actual simulation if something more than the output summary is required. This was designed to run within an mclapply function.


