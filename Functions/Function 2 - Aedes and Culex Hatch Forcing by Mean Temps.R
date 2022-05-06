#' ---
#' title: "Hatching functions"
#' author: "Mindy Rostal"
#' date: "May 4, 2022"
#' output: html_document
#' ---
#' 
#' 
#' Libraries
library(zoo)
library(dplyr)
library(codetools)


#' Functions:
#' Five functions are described in this document.  
#' The first is DayDefinition.  This function finalizes the modification of the dataframe by determining which days have sufficient precipitation and temperature.  
#'    Data going into the function may be a list or a data frame.  However data from a list will not work if the as.data.frame line of code is not at the beginning of the function. 
#' 
#' The AedesForcing Function runs through the nested for() loops that include the switches that determine whether or not the _Aedes_ floodwater mosquitoes and hatch.

#' Parameters needed:
#' ==================
#' 
#' DayDefinition: df, rollcumrainA15, cumrainC15, minrainC, minrainA, Htemp

#' AedesForcingEndemicMeanT: df, hatchabledayA (raindayA)
#' 
#' CulexForcingMeanT: df, dayspostAhatch
#' 
#' dev_CLP: dat, rh025, HA, TH, HH
#' 
#' dev_ALP: dat, rh025, HA, TH, HH


#' Function DayDefinition
#' ======================
#' This function is to finalize the precipitation data by populating the Aedes and Culex wet and warm days.  After this, the data can be put into the Aedes or Culex Forcing functions.

DayDefinition <- function(df, rollcumrainA15,  cumrainC15, minrainC, minrainA, Htemp){# add the function for warm temps later
    df <- as.data.frame(df)   #this helps with formatting and viewing outputs
    df$warmMax <- df$FinalTempCMax > Htemp
    df$warmMean <- df$FinalMeanTemp > Htemp 
    #setup wet days. The `fill` argument gives a vector of the same length with 0 as padding, align = right because you are rolling the data for X days before this date.
    df$CumRainA15 <- rollapply(df$DailyRainfall, rollcumrainA15, FUN=function(x) sum(x, na.rm=TRUE), by=1, by.column = TRUE, fill = 0, align = "right") 
    df$RainedThatDay <- ifelse(df$DailyRainfall >0, 1, 0)
    df$wetA2 <- df$CumRainA15 > minrainA
    #setup wet days. The `fill` argument gives a vector of the same length with 0 as padding, align = right because you are rolling the data for X days before this date.
    df$CumRainC <- rollsum(df$DailyRainfall, cumrainC15, fill = 0, align = "right") 
    df$wetC <-  df$CumRainC > minrainC

    #determine whether Culex hatching is at full force - if the X day cumulative is equal to the maximum X day cumulative for that year, then the Culex eggs hatch at 100%, but it decreases with the recent cummulative rainfall so that on those days the hatching is mediated by a proportion
    
    df<- df%>% group_by(MosqYear)%>%
      mutate(Max10DRainC = max(CumRainC))%>%
      mutate(PropHatchingC = CumRainC/Max10DRainC)%>%
      mutate(PropHatchingC = ifelse(PropHatchingC<.001, 0, PropHatchingC))
    
    df<-as.data.frame(df)
    return(df)
}

AedesForcingEndemicMeanT <- function(df, hatchabledayA){ 
  df0 <- data.frame()
  df$MosqYear<- as.numeric(df$MosqYear)
  df<-rename(df, hatchingATMean = hatchingA)
  
  for(year in unique(df$MosqYear)){
    df1<-filter(df, MosqYear == year)
    df1$HatchingDaysA <- hatchabledayA  #Eggs are only viable once a year for 14 days of rain in the season
    First <- TRUE
    
    for (day in 1:nrow(df1)){
      if(First == TRUE){
        if(df1$warmMean[day] && df1$wetA2[day]){#If it's warm and wet for the first time that season then hatching may proceed for 14 days
          df1$hatchingATMean[day] <- TRUE            #there will be hatching - hatchingA is the variable we will use to force the hatching of Aedes eggs
          df1$HatchingDaysA <- df1$HatchingDaysA - 1  #and we lose a day of hatching for each TRUE hatchingATMean day (e.g. we have one less day to get some viable eggs)
          First <- FALSE
        }else{
          df1$hatchingATMean[day] <- FALSE 
        }
      }else{
        if(head(df1$HatchingDaysA,1) > 0) {
          #Continuing from the first day for the number of hatching days
          df1$hatchingATMean[day] <- TRUE            #there will be hatching - hatchingA is the variable we will use to force the hatching of Aedes eggs
          df1$HatchingDaysA <- df1$HatchingDaysA - 1}else{  #and we lose a day of hatching for each TRUE hatchingATMean day (e.g. we have one less day to get some viable eggs)
            
            df1$hatchingATMean[day] <- FALSE}} 
      
      df1$Days_hatchingA[day] <- df1$HatchingDaysA[day]  
    }
    df0<-rbind(df0,df1)
  }
  remove(df1)
  df0<-select(df0, -HatchingDaysA)
  df<-df0
  return(df)
}

CulexForcingMeanT <- function(df,dayspostAhatch){ 
  df$warmMean <-as.numeric(df$warmMean)
  df$wetC <-as.numeric(df$wetC)
  df<-rename(df, hatchingCTMean = hatchingC)
  df$PropHatchingC <-as.numeric(df$PropHatchingC)
  
  df3 <- data.frame()
  
  for(year in unique(df$MosqYear)){
    CulexCounter<-0 #Culex start hatching at day 7 and peak at day 11-12 vs A. lineapennis that hatch from day 5-13 and A. cumminsii starts hatching on day 1 (Linthicum 1984).
    df4<-filter(df, MosqYear == year)
    First <- FALSE
    for(day in 1:nrow(df4)){
      if(df4$hatchingATMean[day]==TRUE){
        First <-TRUE}
      if(First == TRUE){
        CulexCounter <- CulexCounter + (1 * (CulexCounter < round(dayspostAhatch,0)))}
      if(CulexCounter == round(dayspostAhatch,0)){
        if(df4$warmMean[day] && df4$wetC[day]) {
          df4$hatchingCTMean[day] <- TRUE}}else{
            df4$hatchingCTMean[day] <- FALSE}
      df4$DaysTilHatchC[day] <- CulexCounter}
    First <- FALSE
    df3<-rbind(df3,df4)
  }
  df3$hatchingCTMean<- as.numeric(df3$hatchingCTMean)
  df3<-mutate(df3, hatchingCPropTMean = hatchingCTMean*PropHatchingC)
  return(df3)
}

#'* Number of days between when the *Aedes* and *Culex* hatch
#'* Number of days of cummulative rainfall for RVF epidemic hatching
#'* Amount of cummulative rainfall to permit *Culex* to hatch
#'These are the parameters themselves, not something that we take from the dataframe.
#'
#'Function to calculate daily development rate for Culex
#'From Rueda 1980
dev_CLP <-  function(dat, rh025, HA, TH, HH){
  dat = as.data.frame(dat)
  dat$MeanK = dat$FinalMeanTemp + 273.15 #change to Kelvins
  dat$Dev_CLP = (rh025*((dat$MeanK)/298.15)*exp((HA/1.987)*((1/298.15) - (1/dat$MeanK))))/(1+exp((HH/1.987)*((1/TH)-(1/dat$MeanK))))
  dat$Dev_CLP = dat$Dev_CLP * 0.7650069
  return(dat)
}

dev_ALP <-  function(dat, rh025, HA, TH, HH){
  dat = as.data.frame(dat)
  dat$MeanK = dat$FinalMeanTemp + 273.15 #change to Kelvins
  dat$Dev_ALP = (rh025*((dat$MeanK)/298.15)*exp((HA/1.987)*((1/298.15) - (1/dat$MeanK))))/(1+exp((HH/1.987)*((1/TH)-(1/dat$MeanK))))
  dat$Dev_ALP = dat$Dev_ALP * 1.392704
  return(dat)
}

