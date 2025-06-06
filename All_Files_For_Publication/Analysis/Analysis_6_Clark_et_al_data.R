#####################
### #Calculate the seroprevalence range to use in the study

#####################

#Libraries
library(dplyr)
library(tidyr)

#Data
clark_dat <- read.csv("./All_Files_For_Publication/Data_for_sensitivity_analysis/Clark interepidemic seroprevalences.csv")

#Descriptive calulations
range(clark_dat$Sheep)

#Column summaries
clark_sm_dat <- clark_dat%>%
  select( Country, Sample.size, Camels, Goats, Sheep, cattle)%>%
  mutate(Sample.size = replace(Sample.size, Sample.size == "", "NA"),
         Camels = replace(Camels, Camels == "-", "NA"),
         Goats = replace(Goats, Goats == "-", "NA"),
         Sheep = replace(Sheep, Sheep == "-", "NA"),
         cattle = replace(cattle, cattle == "-", "NA")
         )%>%
  pivot_longer(cols = c(Camels, Goats, Sheep, cattle), names_to = "Species", values_to = "SeroPrev")

clark_sm_dat$SeroPrev <- as.numeric(clark_sm_dat$SeroPrev)

clark_sum <- clark_sm_dat%>%
  group_by(Species)%>%
  summarise(median = median(SeroPrev, na.rm = TRUE),
            range = paste0("[", range(SeroPrev, na.rm = TRUE)[1], ", ", range(SeroPrev, na.rm = TRUE)[2], "]"))


#Estimate 95% interval
sero_prev95 <- NULL   
vec_names <- NULL

for(sp in unique(clark_sm_dat$Species)){
  filt <- filter(clark_sm_dat, Species == sp)
  
  filt_all <- filter(filt, !is.na(SeroPrev))
  
  #Get low 2.5%
  rm_unit <- 0.025*length(filt_all$SeroPrev)
  if(rm_unit <1){
    rm_unit = 1
  }else{
    rm_unit <- as.integer(ceiling(rm_unit))
  }
  
  lo95 <- as.integer(ceiling(min(sort(filt_all$SeroPrev)[-rm_unit])))
  #Get high 97.5%
  hi95 <- sort(filt_all$SeroPrev)[0.975*length(filt_all$SeroPrev)]
  #output
  sero_prev95 <- c(sero_prev95, paste0("[", lo95, ", ", hi95, "]"))
  
  #Species name
  vec_names <- c(vec_names, sp)

}

sprev_dat <- as.data.frame(cbind(sero_prev95, vec_names))

sprev_dat <- rename(sprev_dat, Species = vec_names)

clark_sum_tot <- full_join(clark_sum, sprev_dat)

##Add all species
clark_sm_dat_all <- filter(clark_sm_dat, !is.na(SeroPrev))
#Get low 2.5%
rm_unit_all <- 0.025*length(clark_sm_dat_all$SeroPrev)
if(rm_unit_all <1){
  rm_unit_all = 1
}else{
  rm_unit_all <- as.integer(ceiling(rm_unit_all))
}

lo95_all <- as.integer(ceiling(min(sort(clark_sm_dat_all$SeroPrev)[-rm_unit_all])))
#Get high 97.5%
hi95_all <- sort(clark_sm_dat_all$SeroPrev)[0.975*length(clark_sm_dat_all$SeroPrev)]
#output
sero_prev95_all <-  paste0("[", lo95_all, ", ", hi95_all, "]")
sero_prevrange <- paste0("[", range(clark_sm_dat_all$SeroPrev, na.rm = TRUE)[1], ", ", range(clark_sm_dat_all$SeroPrev, na.rm = TRUE)[2], "]")


all_row <- cbind("All species", median(clark_sm_dat_all$SeroPrev, na.rm = TRUE), sero_prevrange, sero_prev95_all)
all_row <- as.data.frame(all_row)
names(all_row) <- colnames(clark_sum_tot)


clark_sum_tot <- rbind(clark_sum_tot, all_row)
