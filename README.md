# Localized Rift Valley Fever Virus Persistence

This repository contains code, data, and figures that support:

Rostal MK, Prentice J, Ross N, Kemp A, Thompson PN, Anyamba A, Cleaveland S, Cordel C, Msimang V, van Vuren, JP, Haydon D, Karesh WB, Paweska JP, Matthews L. (2025) Localized Rift Valley Fever Virus Persistence Explains Epidemic and Interepidemic Dynamics and Guides Control Strategies. *Proceedings of the Royal Society B: Biological Sciences* DOI: 10.1098/rspb.2025.0453


---

###  RVF_SIR_Model

Repository of code to simulate 34-years of RVFV transmission at a single pan in central South Africa.

All code referred to below are in the *"All_Files_For_Publication" folder*. To run the simulations, run the scripts in order according to the file number. If the file name starts with the term Function or Model, then it should not be run by the user and will be called by the main scripts. If a file name starts with the term Analysis, then it may be run by the user depending on the level of analysis they would like to do. The main scripts will allow the user to run the 34-year simulation with the option of various scenarios (e.g., vaccination), run the Latin hypercube sensitivity analysis, run the variable-by-variable analyses and estimate R_0. The analysis scripts will produce plots and analyze the Latin hypercube results. The model scripts contain the parameter list, the mosquito hatching determination and the ODE function. The function scripts have all other functions defined for the analyses. 

**1. Run the simulation:**

Start with the _“1-Run RVFV Simulations.R”_ file. The user will start by indicating whether they would like to run a specific scenario (with vaccination, without transovarial transmission (Only.q = no horizontal transmission or no.amp.vec = no transmission by _Culex_), without horizontal transmission (No.q), a low starting population of infected _Aedes_ eggs (lo.eggs), and with a higher (muC.25_higher) or lower _Culex_ (muC.25_lower) mortality rate). If the vaccination scenario is selected the user must also indicate the coverage (proportion of animals in which we maintain immunity via vaccination, (vax.proportion))  and may select scenarios with a 3 times higher (vax.25_higher) or 25% lower vaccine proportion (vax.25__higher_lower) from what was selected.  To run these additional vaccine scenarios both “Vaccinate”, and the proportion higher or lower must be set to “TRUE”. If all scenarios are set to FALSE, then the full, exemplar simulation will be run. Note that the SA scenario should always be FALSE, as there is a separate code to run the Latin hypercube sensitivity analysis.

Source all required code. If a scenario is selected, then the code will adjust the parameter list as required.

The simulation is run, and results are converted to a data frame.

The Define.MosqYr adds the annual season (allowing the year to be assessed from September-August according to the mosquito season (MosqYr) rather than the calendar year).

Three plots are made for a quick spot check. SLplot shows the host dynamics, MosqIAEplot plots the population of infected _Aedes_ eggs, MosqIAll plots the infected _Aedes_ and _Culex_ adults and PeakIMosq_Plot plots the ratio of infected _Culex_ to _Aedes_ vs infected livestock. A plot of the effective R0 is also included. For the vax.burst scenarios, the results for most plots are written as a csv file to facilitate combining of these plots for the publication. To make the plots from the publication, run the multiplot code (“Analysis 1 Code for multiplots.R”).

The code also calculates some metrics and provides a data frame of the number of infected hosts per season. Metrics include size of annual outbreaks, summarize the number of infected _Culex_ and _Aedes_ at the end of the year, calculate the mean larval/pupal development and mortality rates (which varies daily dependent on temperature), calculate the mean population sizes (for mosquitoes this only includes days when there is at least one adult mosquito present) and the mean mosquito population during each season’s peak for each species.

The code produces Table S1, which includes the mean seroprevalence, the mean annual maximum host-vector ratio, mean annual proportion of infected _Aedes_ eggs, mean annual proportion of infected adult _Aedes_, and the mean annual proportion of infected adult _Culex_. The table will be saved if no other scenarios are selected (e.g., all scenarios are set to FALSE).

The code saves data from the following scenarios for use in the "Analysis_1 Code for multiplots.R" file: No.q.

Calculate the effective R0 for each time step of the simulation.

Files sourced:

* "Function 1 Define Functions for Output of Sensitivity Analysis.R"
* "Function 3 Plot simulations for a given time period.R"
* "Function 4 Calculate R0.R"
* "Function 5 Calculate R0 dfs for plot.R"
* "Model 2 Mosquito hatch rates at daily timestep.R", which includes:
    * "Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R"
    * "Model 1 - RVFV Optimized Parameters for publication.R", which is sourced within "Model 2 Mosquito hatch rates at daily timestep.R"
* "Model 3 RVFV ODE SIRS function.R"

**2. Run the Latin hypercube sensitivity analysis:**

*"2_RVF_LHC_Sensitivity_Analysis.R"*.
Ensure that SA is always set to TRUE (this ensures that the parameter ranges for the sensitivity analysis in the parameters code file is run). Set h as the number of simulations to run. It is automatically set to 4000, which is the number used in the publication. Please note that with this large number of simulations, the analysis takes a significant amount of computing power and time. Var_Select should always be set to FALSE. Source all functions and climate data.

The Latin hypercube function was designed to run in parallel on multiple cores. The code was set to either run on 20 cores or if there are more cores than number of simulations to run, then use one core per simulation to maximize efficiency.

The hypercube parameters are inputted as a list and split by row. Two blank data frames need to be entered into the function so that they can be used in the function.

The results from the function are turned into a list and bound by rows, then the columns are named for each output. A file name is created using the current date and the data is saved.

Files sourced: 

* "Function 1 Define Functions for Output of Sensitivity Analysis.R"
* "Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R"
* "Function 6 Latin Hypercube Analysis.R"
* "Model 1 - RVFV Optimized Parameters for publication.R"
* "Model 3 RVFV ODE SIRS function.R"


**3. Run the R0 analysis**

Run the *"3- Calculate and Plot_R0.R"* code. This code will calculate R_0 for various populations and then plot the sensitivity analyses. Use the function lst.R0.var (defined in "Function 5 Calculate R0 dfs for plot.R") to produce a list of data frames, where each data frame has the value of R_0 for each population size as you vary the variables listed in R0_vec. Calculate the value of R_0 using the values of the parameters used in the full simulation for the peak_popn, peak_A_only and peak_C_only and store in a data frame.

The population vectors are defined for the hosts and vectors as follows:
mean_popn: All population means calculated across the entire simulation – the mean population of adult sheep, lambs and the mean population (when at least one adult mosquito of the species is present) of _Aedes_ and _Culex_. 
peak_popn: The mean population size of the sheep and lambs calculated across the entire simulation. For the vectors, the maximum population size during each year of the simulation (peak) was averaged across the 34 years of the simulation. 
max_popn: The mean population size of the sheep and lambs calculated across the entire simulation. For the vectors, the maximum population size over the full simulation was used.
peak_A_only: The mean population size of the sheep and lambs calculated across the entire simulation. For the _Aedes_, the maximum population size during each year of the simulation (peak) was averaged across the 34 years of the simulation. No _Culex_ are included.
peak_C_only: The mean population size of the sheep and lambs calculated across the entire simulation. For the _Culex_, the maximum population size during each year of the simulation (peak) was averaged across the 34 years of the simulation. No _Aedes_ are included.
no_mosq: The mean population size of the sheep and lambs calculated across the entire simulation. No _Aedes_ or _Culex_ are included.

Produce the plots (Fig S4) showing how R_0 varies as you change a given parameter using the following vector populations: at the peak population, the peak population of _Aedes_ only and the peak population of _Culex_ only. The parameters plotted, in order are Tsal, muA, biteA, q, Tcsl, muC, and biteC. The parameters need to be plotted in the order of the R0_vec. This produces the initial plots using the mean peak vector populations.

Run the "Analysis 5 Plot R0 changes at peak and mean populations.R" file to add the lines for the mean population levels and complete Figure S4.

Plot Figure S5 in which we vary each parameter individually to be the value in the model times a factor (0.5, 0.75, 1, 1.25, 1.5) and calculate R0. Then create a box plot for the range of R0 for each parameter.

Finally, calculate the R0 estimate at different populations.

Files sourced:

* "1- Run RVFV Simulations.R"
* "Function 4 Calculate R0.R"
* "Function 5 Calculate R0 dfs for plot.R"
* "Analysis 5 Plot R0 changes at peak and mean populations.R"


### Analysis Code

**A1. Analyze the simulation results**

*“Analysis/Analysis 1 Code for multiplots.R”* 
Create plots of the simulation and scenarios. Run the code. This code will produce Figures 1-4 ; S2-S4; S6-S7 and S9-S12.

If the plots should be saved (and the plots for Figure 1 combined) set Multiplot to be TRUE. Set the plot theme elements to be used for all plots.

Make plots of the _Aedes_ mosquito, _Culex_ mosquito, infected _Aedes_ egg, and all infected mosquito populations as well as of the host population. The outbreaks reported during the simulation time period in Pienaar et al (2013) are listed and those that were in the high veld area (Free State/Northern Cape Provinces) or were reported but from an unknown location are identified and included on the host plot. A plot of the effective R_0  on each time step of the simulation is also produced. 

* Figure 1. If no scenarios are selected and Multiplot is TRUE, the code will combine the population plots and the effective R_0 plot to make Figure 1. As are Figure2 and Supplementary figures S2, S3, S7, S8 and S12 are also made if no scenarios are selected. Figures S9 is compiled when no scenarios are selected, but plots S9a and S9c must have already been created by running those scenarios.
* Figure 2 is made (assuming no scenarios are selected) using saved data from the R_0 analyses ("Data_for_sensitivity_analysis" folder), looking at transovarial transmission vs _Aedes_ bite rate and the impact on R_0 and seroprevalence. Similarly, Figures S7 and S8 are made looking at similar outcomes as transovarial transmission and _Culex_ mortality rate changes.
* Figure 4 is made when the Vaccinate is set to TRUE and no specific vaccination scenarios are selected. It produces the host and infected mosquitoes (A and B) and collates the plots with those of the vaccination vs extinction with low and high transovarial transmission (TOT), which is made using the data saved in the "Data_for_sensitivity_analysis" folder and produced in the "Analysis_2_Plot_seroprev_as_key_params_change.R" code. Figure S11b is also saved.
* Figures S2 (rainfall and temperature) is produced if no scenarios are selected.
* Figure S3 (infected hosts vs the ratio of infected _Culex_ to _Aedes_) is made if no scenarios are selected
* Figures S4 & S5 are not coded here. The code for these plots are in the Calculate and Plot_R0.R file.
* Figure S6 is compiled if the scenario with no horizontal transmission (Only.q; S4B) is selected and calls the saved No.q scenario data from the "Data_for_sensitivity_analysis" folder, if the scenario with no transovarial transmission (No.q scenario) has already been run. (data is saved for S4A). The two plots are compiled when the no horizontal transmission scenario (Only.q) is selected.
* Figure S7 is made using data from the sensitivity analysis where we compare changes in simulated seroprevalence and persistence as a single (S7a and S7b) variable is varied or as two variables are varied (S7c and S7d). The data for these plots is sourced from the "Data_for_sensitivity_analysis" folder and created by "Analysis_2_Plot_seroprev_as_key_params_change.R" and "Analysis_3_Plot_outcome_vs_single_key_param.R" files.
* Figure S8 is also made from data that is sourced from "Data_for_sensitivity_analysis" and created by "Analysis_2_Plot_seroprev_as_key_params_change.R".
* Figure S9. If the scenario with the 25% lower _Culex_ mortality rate is TRUE (muC.25_lower = TRUE) then Figure S9a is made). If the scenario with the 25% higher _Culex_ mortality rate is TRUE (muC.25_higher = TRUE) then Figure S9c is made). 
* Figure S9b (the simulation run without changing the _Culex_ mortality rate) and the complied figure of S9 are made if no scenarios are being run AND Figures S9b and S9c have already been made.
* Figure S10-right and S10-left are  plotted if both Vaccinate is set to TRUE and vax.25_higher (S10 - right) or vax.25_lower (S10 - left) are set to TRUE, respectively. The plots are arranged and saved. Figure S10 is compiled with the previously saved plots when Vaccinate is set to TRUE and no scenarios of 25% higher or lower are selected and vax.burst is set to FALSE.
* Figure S11 is a plot of the scenario where no _Culex_ are permitted to transmit (no.amp.vec == TRUE). _Aedes_ are able to transmit both vertically and horizontally.
* Figure S12 is produced if no scenarios are selected and shows the population dynamics of _Aedes_ and _Culex_ over a randomly selected year.


Files sourced:
* "Function 3 Plot simulations of a given time period.R"
* "And multiple data files"

**A2. Examine how the simulated seroprevalence and persistence change as the values of select parameter pairs are varied.** 

Run *"Analysis/Analysis_2_Plot_seroprev_as_key_params_change.R"*. Note, depending on how many values are being evaluated for each parameter this can take significant computing power and time to run. The code can be run for the five variable pairs and corresponding outcomes described in the paper. Set one of the five to TRUE and keep the others FALSE:

* Figure S4c data: Q_biteA_Seroprev.mean: Transovarial Transmission vs _Aedes_ bite rate evaluating seroprevalence
* Figure S4d data:Q_Tasl_Persistence: Transovarial Transmission vs host-to-_Aedes_ transmission rate evaluating persistenceQ_muC_Seroprev.mean: Transovarial Transmission vs _Culex_ mortality rate evaluating seroprevalence
* Figure 4c data: Vax_Q_Persistence: Transovarial Transmission vs vaccination rate evaluating persistence
* Figures 2 and S8: Q_biteA_Seroprev.mean AND R0_plot ==TRUE evaluates scenarios examining the effects of seasonal R0 for the following outcomes: Mean seroprevalence, persistence (the time step after which there are no more infected hosts) and R0 (calculated at the mean host and vector populations).
* Q_IAE_Persistence: Transovarial Transmission vs the initial infected egg population evaluating persistence (this data was not used as a figure)


The values for each variable that were evaluated for the manuscript are given next. Then variables, functions and outcomes are defined for each specific variable pair. A blank data frame (summary.dat) is created and a counter parameter (j) is set. 

A for-loop runs through every combination of parameter pairs. For the variable pair that includes vaccination the initial populations are adjusted such that the designated proportion of the total sheep and lambs are vaccinated, and these are removed from the susceptible population.

The model is then run, and the output database is modified.

If the R0_plot is selected, then estimate the mean population sizes and development rate and calculate R0.

Everything is added to the summary.dat data frame.

If the R0_plot is selected, and after all of the simulations in the for loop are completed, add a column with the value of R0 calculated when the transovarial transmission fraction is set to zero by filtering and merger the data frame. Calculate the seasonal and long-term persistence.

Name and save the .Rdata and .csv files.

Produce plots (also produced in the "Analysis 1 Code for multiplots.R" file)

Files sourced:
* "Function 1 Define Functions for Output of Sensitivity Analysis.R"
* "Function 3 Calculate R0.R"
* "Model 2 Mosquito hatch rates at daily timestep.R"
* "Model 3 RVFV ODE SIRS function.R"

**A3. Analyze the effects of varying select parameters on the simulated seroprevalence and persistence.**

Run the *"Analysis/Analysis_3_Plot_outcome_vs_single_key_param.R"* file. It is designed to vary one of four parameters: transovarial transmission, host-to-_Aedes_ transmission, _Aedes_-to-host transmission, and extrinsic incubation period. The following must all be set to false: SA, Var_Select, Vaccinate, No.q, Only.q. It can run one parameter at a time, which is set to TRUE at the beginning of the code. For each parameter, a simulation will be run for every value between 0 and 1 at 0.5 intervals and for the value used in the full model (except Check_vax, which has additional values). An if_else statement has a separate code for Check_Vax as this requires changing the initial population values and includes the day that RVFV goes functionally extinct in the _Aedes_ egg population. After each simulation the mean seroprevalence and days of persistence are estimated. The data are then saved and single parameter plots against seroprevalence and persistence are produced and saved.

Files sourced:
* "Function 1 Define Functions for Output of Sensitivity Analysis.R"
* "Function 3 Plot simulations of a given time period.R"
* "Model 3 RVFV ODE SIRS function.R"
* "Model 2 Mosquito hatch rates at daily timestep.R"

**A4. Analyze the data from the sensitivity analysis:**

Run the *"Analysis/Analysis_4 Assessing the sensitivity analysis.R"* file. This file will produce the data for and plot Figure 3 from the output of the Latin hypercube sensitivity analysis ("2_RVF_LHC_Sensitivity_Analysis.R"). We also calculated the proportion of simulations that: become extinct, have a high seroprevalence, or persist and are within our target seroprevalence. 

To make the data for the plot, vectors are made of the parameters to compare and the outcomes. The data is subset to just the parameters being evaluated. Estimate the partial correlation coefficient for each variable. The estimates and confidence intervals are used to make the tornado plot. Save the plot.

**A5. Add lines of the effect that varying the selected parameters have on the estimate of R0 at mean host and vector populations**

Run the *"Analysis/Analysis_5 Plot R0 changes at peak and mean populations.R"* file (note: this file can only be run after "3- Calculate and Plot_R0.R"). First make a list of data frames that gives the value of R0 for the three population sizes (mean_popn [mean host and vector populations], mean_A_only [mean host and _Aedes_ populations] and mean_C_only [mean host and _Culex_ populations]. Then make a vector of the R0 value of the parameters used in the full simulation. 

Figure S4. Make a plot of the R0 using mean and mean peak vector populations as each parameter is varied (Tsal, muA, biteA, q, Tcsl, muC, and biteC.). Combine each all of the plots and save the final figure.

**A6. Analyze the data from the sensitivity analysis:**

Run the *"Analysis/Analysis_6_Clark_et_al_data.R"* file. This file has the code for calculating the target seroprevalence range. The data used was part of the supplementary data provided by [Clark et al. 2018](https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0006627) with minor corrections made to the data set as described in the supplemental methods.  The script then calculates the median, range and 95% confidence intervals for the seroprevalence estimates for each species and for all species included.

**A7. Analyze the data from the sensitivity analysis:**

Run the *"Analysis/Analysis_7 Automatically plot all results.R"* file. This file will run all the files and scenarios to make all of the plots from the publication, without the need to manually select a scenario. Initially all scenarios are set to FALSE and should stay that way. The the script runs through the vaccination scenarios and resulting plots, then it runs through the remaining scenarios and creates the resulting plots, finally it sources to code to make the plots from other analysis files.


### Model Scripts

**M1. Optimized parameters used in the simulation.**

File: *Model 1 - RVFV Optimized Parameters.R*

This file is sourced within the "Model 2 Mosquito hatch rates at daily timestep.R" file. All model parameters are defined and placed into a vector for the full simulation, the R0 analysis and the sensitivity analysis. Full definitions and references are provided in Table S4. Different parameters and ranges are used for the exemplar model and the parameter selection process - for details see 0_RVF_LHC_to_select_params below.

**M2. Define mosquito hatching days and daily rates.**

File: *Model 2 Mosquito hatch rates at daily timestep.R*

This file pulls the climate data and uses functions (see below) to identify days in which the _Culex_ and _Aedes_ will hatch. Then it uses the approxfun() function to be able to feed the daily hatching data directly into the differential equation.

Files sourced:
* Data: "Combined Temp and Precip Data for RVF Simulation.csv"
* "Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R"
* "Model 1 - RVFV Optimized Parameters.R"

**M3. ODE function with full set of differential equations to define model.**

File: *Model 3 RVFV ODE SIRS function.R*

This file describes a function that takes a population vector, a series of time steps, a parameter vector (as a list), vectors to allow daily control over hatching, development and vaccination rates, and the end time of the model. It describes the differential equations used to run the simulation (also described in the supplemental text Equations S1-S30) and which derivatives and populations to return.

### Function Codes:

**Function 1 Define Functions for Output of Sensitivity Analysis.R**

A set of functions written to assess the outputs of the sensitivity analysis as well as other useful calculations. The following functions are defined:

* _Define.MosqYr()_ is a function that adds the seasonal (“MosqYear”/Month) info to the population data frame. The MosqYear is defined as being from September to August (e.g., September 1982-August 1983 = MosqYear 1982). It takes the following arguments: the simulation population data frame (df) and the precipitation/temperature data frame (precip.df). 
* _Get.Outbreak.Dataframe()_ is a function that makes a data frame of all outbreaks by year/season. It takes the following arguments: the simulation population data frame (df) and the sheep/lamb recovery rate (sigma_sl).
* _InfectedMosqPeak()_ is a function that produces a data frame indicating when and the magnitude of the largest population of vectors occurs each year. It takes the following argument: the simulation population data frame (df). The data frame has one row per simulation season (MosqYear). For the populations of infected _Aedes_, infected _Culex_, Susceptible _Aedes_ and Susceptible _Culex_ it indicates the time step (“SimDay”) and the size of the maximum population on a single time step during that season. It indicates which species has a higher peak of infected mosquitoes that season and the size of the maximum population of infected _Aedes_ eggs that season. It calculates the ratio of the peak _Culex_ and _Aedes_ population for that season (note these will not have occurred on the same day). Finally, it binds the outbreak data frame to the peak infected mosquito data frame indicating the number of hosts infected during that season.   
* _Get.Mean.Vec.Pops_ is a function that calculates the mean of the vector populations (across time steps where there is at least one adult mosquito of that species present). It takes the following arguments: the simulation population data frame (df) and the population parameter of interest (e.g., “NAedes”). It outputs three estimates: the overall mean population size (m_mosq_pop), the maximum peak population, which is the maximum seasonal population size (per “MosqYear”) of the simulation (max_mosq_pops), and the mean peak population, which is the mean of the seasonal vector populations during a 34-season simulation (m_peak_Mosq_pop). 
* _Get.Mean.SeroP.MosqY_ is a function that first identifies the largest proportion of recovered hosts (the seroprevalence) during that season (MosqYear), then calculates the mean of the seasonal proportions during the 34-year simulation. It takes the following arguments: the simulation population data frame (df).
* _Get.Extinct.Day_ is a function that identifies the time step after which no hosts are infected (IS and IL are both <1). Extinction in the host is our conservative definition for RVFV extinction. Thus, this function identifies how long the virus persists in the host populations. It takes the following arguments: the simulation population data frame (df).
* _Get.Extinct.IAE.Day_ is a function that identifies the time step after which no _Aedes_ Eggs are infected (IAE is <1). There are two types of extinction in the eggs, the measure we use is functional extinction (e.g. the rate of infected mosquitoes is so low that the hosts don't become infected). Based on simulation with various initial population sizes for the infected eggs using the lo.IAE scenario in which it was determined that the minimum population was 14,400 is our conservative definition for RVFV extinction. Thus, this function identifies how long the virus persists in the host populations. It takes the following arguments: the simulation population data frame (df).
* _Get.Max.Outbreak.Size.MosqYear_ is a function that sums the total number of infected hosts per season (MosqYear). During the calculation it is assumed for any host population, that if the number of infected hosts (IS and/or IL) is < 1, there are effectively 0 infected hosts in that population during that time step. It then sums the number of infected animals for each season (divided by the recovery rate as an individual will stay infected for more than one time step. The function outputs the number of infected hosts during the largest seasonal outbreak of the simulation. It takes the following arguments: the simulation population data frame (df) and the recovery rate (sigma).
* _Get.Mean.Outbreak.Size.MosqYear_ is a function that sums the total number of infected hosts per season, as described for the Get.Max.Outbreak.Size.MosqYear() function, then takes the mean across all of the seasons of the simulation (34) to provide the average size of seasonal outbreak. It takes the following arguments: the simulation population data frame (df) and the recovery rate (sigma).
* _Get.Mean.Outbreak.lengths.MosqYear()_ is a function that sums the number of days an infected host is present during the season. Then it takes the mean across the seasons of the simulation to provide the average outbreak length per season. It takes the following argument: the simulation population data frame (df).
* _Get.Prop.Infected.Eggs()_ is a function that estimates the proportion of infected eggs on the last time step of the 34-year simulation. It also estimates the range and mean of the proportion if infected eggs across the full simulation period. It takes the following argument: the simulation population data frame (df). It outputs four elements: mean.prop.i (the mean proportion of infected eggs over all time steps), max.prop.i (the maximum proportion of infected eggs across all time steps), min.prop.i (the minimum proportion of infected eggs across all time steps), end.prop.i (the proportion of infected eggs at the final time step of the simulation).
* _Get.Prop.Infected.Mosq()_ is a function that estimates the proportions of the infected population of any vector population (e.g., IA, IC, IAE). It takes the following arguments: the simulation population data frame (df) and the name of the column with the infected vector population of interest (i_pop; e.g. “IAE”). It calculates the proportions for time steps when at least one adult mosquito of the given vector population is present and for the entire 34-year simulation. It outputs three elements: mean.prop (the mean proportion of infected eggs over all time steps), range.prop.low (the minimum proportion of infected eggs across all time steps), range.prop.hi (the maximum proportion of infected eggs across all time steps).


**F2. Define _Aedes_ and _Culex_ hatching days.**

File: *Function 2 - Aedes and Culex Hatch Forcing by Mean Temps.R*

This file contains 5 functions to define the hatching days for the vector populations and to calculate the development rates for the _Aedes_ and _Culex_ larval/pupal stages (_DayDefinition_, _AedesForcingEndemicMeanT_, _CulexForcingMeanT_, _dev_CLP_, _dev_ALP_). The first three functions must be run on the climate data frame in order (_DayDefinition_, then _AedesForcingEndemicMeanT_, and finally _CulexForcingMeanT_).
* _DayDefinition_: takes the following parameters: df (a climate data frame), rollcumrainA15 (number of days to include in estimate of the cumulative rainfall for _Aedes_ hatching), cumrainC15 (number of days to include in estimate of the cumulative rainfall for _Culex_ hatching), minrainC (the minimum amount of precipitation for _Culex_ hatching), minrainA (the minimum amount of precipitation for _Culex_ hatching). To help account for variability in hatching in _Culex_, which is based on standing water and for which we do not have data, we adjusted the proportion of eggs that hatch is given by taking the 3-day cumulative and dividing it by the maximum 3-cumulative precipitation that season. This allows more hatching when there is more precipitation. After running this function, each time step is defined in terms of whether it surpasses the thresholds set by temperature and precipitation for hatching for _Aedes_ and _Culex_ and what proportion of _Culex_ eggs may hatch.
* _AedesForcingEndemicMeanT_: takes the following parameters: df (a climate data frame),  hatchabledayA (Eggs are only viable once a year for 14 days with precipitation in the season). The function adds a column to the data frame to dictate when _Aedes_ egg hatching is permitted (on the first 14 days of the season that surpass both the temperature and precipitation thresholds).
* _CulexForcingMeanT_: takes the following parameters: df (a climate data frame), dayspostAhatch (the number of days after the _Aedes_ start hatching that the _Culex_ can begin hatching). The function adds a column to the data frame to dictate when _Culex_ egg hatching is permitted (on days of the season that surpass both the temperature and precipitation thresholds that occur after the _Aedes_ are finished hatching).
* _dev_CLP_: takes the following parameters: dat (a climate data frame), and species-specific parameters defined in Rueda et al 1980 (rh025, HA, TH, HH) to calculate the development of _Culex_ larva/pupae. The calculation is found in Rueda et al 1980.
* _dev_ALP_: takes the following parameters: dat (a climate data frame), and species-specific parameters defined in Rueda et al 1980 (rh025, HA, TH, HH) to calculate the development of _Aedes_ larva/pupae. The calculation is found in Rueda et al 1980.

**F3. Functions to support plotting.**

File: *Function 3 Plot simulations for a given time period.R*

This file describes four functions: _ParamByParamPlot_, _plot.between.years_, _SL_plotter_ and _IAE_plotter_.

* _ParamByParamPlot_ takes the following parameters df1 (data frame with the results of the parameter by parameter simulation ("Analysis_2_Plot_seroprev_as_key_params_change.R")), colx (the parameter to be plotted on the X-axis), coly (the outcome factor to be plotted on the Y-axis), color1 (the second parameter being evaluated and presented as discreet data in the plot), no.leg (TRUE or FALSE, whether a legend should be plotted or not). This function is used to plot the parameter-by-parameter analyses included in Figures S7 and S8. 
* _plot.between.years_ takes the following parameters: df1 (the simulation data frame), yr1 (the lower bound of year range), yr2 (the upper bound of year range). This function is used to plot the simulation results between a specific subset of years (e.g., Figure S6 plots the simulation results between 2004-2006; also used in Figure S12).
* _SL_plotter_ takes the following parameters: df (the simulation data frame), legnd (a logical parameter indicating whether or not a legend should be included on the plot). This function is used to plot the sheep and lamb (host) dynamics plot (e.g. Fig 1a) and is primarily used for Fig S11.
* _IAE_plotter_ takes the following parameters: df (the simulation data frame), legnd (a logical parameter indicating whether or not a legend should be included on the plot). This function is used to plot the infected _Aedes_ eggs dynamics plot (e.g. Fig 1c) and is primarily used for Fig S11.

**F4. Functions to calculate R0 and effective R0**

File: *Function 4 Calculate R0.R*

This file has two functions: _calc_R0_ and _R_eff_. 

_calc_R0_ takes the following parameters: parameters (a vector of parameters used in the simulation, from "Model 1 - RVFV Optimized Parameters.R"), pop_size (vector of the host, _Aedes_ and _Culex_ populations). The R0 analysis was done in wxMaxima, which allowed a numeric solution to estimated. The algebraic equations and the transmission and transition matrices were defined by the wxMaxima analysis, then input into this function to allow the quick calculation of R0 when varying the parameters and the population sizes in the R ecosystem. 

First the larval/pupal mortality rates are calculated (from the mean development rates) and the transmission terms are calculated and combined. The proportions of susceptible sheep, lambs _Aedes_ and _Culex_ are estimated using equations produced by the wxMaxima analysis. Likewise, the adjusted birth rate for the system is calculated (using equations produced by the wxMaxima analysis). 

Next generation matrices were defined for a small domain K_S (using only the rows and columns of K_L belonging to new states-at-infection) to calculate R_0. We calculated the Jacobian matrix and decomposed it into the T matrix (the transmission parameters that describes the production of new infections) and Σ matrix (the development part that describes changes in state). The T and Σ matrices were transcribed from the wxMaxima analysis into the R analysis.

_R_eff_ takes the following parameters: pop_matrix (a of populations, including those of the susceptible populations of hosts, _Aedes_ and _Culex_ populations and the MosqYear – must be in order SS, SL, SA, SC), params (a vector of parameters used in the simulation (from "Model 1 - RVFV Optimized Parameters.R")) and get.fun1 (the R0 calculation function to run _calc_R0_). The function calculates the R0 for that population of susceptible hosts and vectors.

**F5. Create data frames for the sensitivity analysis of R0 for RVFV.**

File: *Function 5 Calculate R0 dfs for plot.R*

This file has one function in it: _lst.R0.var_. The function takes the following parameters: empty.dat (an empty database), empty.lst (an empty list), the host/vector populations to be evaluated - both.pop, A.pop, C.pop (indicate which vector of populations for the hosts, _Aedes_ and _Culex_ populations (both.pop), hosts and _Aedes_ (A.pop) and hosts and _Culex_ (C.pop) [e.g. mean_popn vs peak_popn]), R0params.func (the optimized parameters used in the R0 calculation (from "Model 1 - RVFV Optimized Parameters.R"), get.fun.1 (the function to run, e.g. _calc_R0_ from "Function 4 Calculate R0.R") and var_vec (the list of variables that will be evaluated). The function will produce a list of data frames, where each data frame has the value of R0 for each population size as you vary the variables listed in var_vec. The calculated data frame is used to make the plots for Figure S7.

**F6. Conduct the Latin Hypercube analysis**

File: *Function 6 Latin Hypercube Analysis.R*

The function, _HypercubeSA_, with 21 inputs, is described. The main simulation code from "1-Run RVFV Simulations.R" and "Model 2 Mosquito hatch rates at daily time step.R" included in the function. After the simulation is run, six outcome functions are run. They are detailed when the function is called, but must be left in order as some require specific arguments and these are hard coded into the function. Further, they need to be in order so that the column names applied after the function is run labels the results correctly. If needed an option to save each simulation data file can be uncommented and used to evaluate the actual simulation if something more than the output summary is required. This was designed to run within an mclapply function.

### Selecting parameters, if desired

**0. Parameter selection code:**

The above code and scripts run using the parameters for the exemplar simulation (or specific scenarios that change individual parameters. There were six parameters for which there were no reliable estimates available: transovarial transmission, the mortality rate of _Aedes_ eggs, _Aedes_ and _Culex_ bite rates, and _Aedes_ and _Culex_ carrying capacity. To identify realistic estimates, a Latin hypercube sampling method was used to evaluate 80,000 combinations of these six parameters.

This code is written in the file: *0_RVF_LHC_to_select_params.R*.

**A5. Analyze the parameter combinations resulting from the simulation**

*"Analysis_5 Plot R0 changes at peak and mean populations.R"* evaluates the results of the 80,000 iterations of the Latin hypercube using six criteria:  mean annual seroprevalence was between 5-40%, the virus persisted for the entire simulation, at least one outbreak “spike” was detected, the ratio of infected _Aedes_ eggs remain relatively constant during the simulation and the mean seroprevalence remained relatively constant during the simulation. An outbreak was considered a “spike” if the ratio of infected animals in that year compared to the average number of infected animals in the three years before and after was 2:1 or greater. The population of _Aedes_ eggs and mean seroprevalence was determined to be “relatively constant” if the ratio of the mean value from the first half of the simulation to that of the second half of the simulation was within 0.9-1. For the infected _Aedes_ eggs we averaged the annual maximum number of eggs. For the seroprevalence we averaged the annual mean seroprevalence. The iterations that met the 6 criteria were visually assessed and the closest match to the historical RVFV pattern was selected. The parameter values for the six parameters were then input into the parameter list ("Model 1 - RVFV Optimized Parameters.R)".

