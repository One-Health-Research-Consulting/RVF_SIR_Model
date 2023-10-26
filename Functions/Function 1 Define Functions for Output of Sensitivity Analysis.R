#' ---
#' title: "Define Functions for output of Sensitivity Analysis"
#' author: "Mindy Rostal"
#' date: "May 5, 2022"
#' ---
library(stringr)
library(rlang)

#Add MosqYear/Month info to final.populations database 

Define.MosqYr <- function(dat, precip.df){
  is = dat$IS
  il = dat$IL

  if(any(is.na(is))| any(is.na(il))){ #If there are NAs
    return(print("IS, or IL was NA")) #return that it was NA
  }else{
    #Need to use Mosq year 
    mosq.yr = select(precip.df, SimDay, MosqYear, MosqDay, Month) #select columns to indicate mosqYear and precip differences
    #Change SimDay to time to match the final.populations dataframe
    mosq.yr = rename(mosq.yr, "time" = SimDay)
    df = left_join(dat, mosq.yr, by = c("time"))
    df = arrange(df, time)
    df = df%>%
      mutate(MosqYear = replace(MosqYear, is.na(MosqYear), 2017))
      }
  return(df)
}

#Get dataframe of outbreaks by year
Get.Outbreak.Dataframe <- function(df, sigma_sl){
  is = df$IS
  il = df$IL
  if(any(is.na(is))|any(is.na(il))){#If any infectious animals are NA
    return(paste("IS or IL was NA"))#Return NA message
  }else{
    df$Month <- as.numeric(df$Month)#Change class
    empt.df = df%>%
      group_by(MosqYear, Year, Month)%>%
      mutate(IS_count = if_else(IS <1, 0, IS))%>%
      mutate(IL_count = if_else(IL <1, 0, IL))%>%
      summarise(Endemic = round(sum(IS_count, IL_count, na.rm = TRUE)/(1/sigma_sl),3))
    
    empt.df = empt.df%>%
      group_by(MosqYear)%>%
      summarise(Endemic = sum(Endemic, na.rm = TRUE),
                Month = round(mean(Month, na.rm = TRUE),3),
                Yr = round(mean(Year, na.rm = TRUE),3))%>%
      mutate(Month = replace(Month, is.na(Month), 0))%>%
      mutate(Yr = replace(Yr, is.na(Yr), 0))%>%
      mutate(Month = str_pad(Month, 2, side = "left", pad = 0))%>%
      mutate(Outbreak_Mo_Yr = paste(Yr, Month, sep = "_"))%>%
      mutate(Outbreak_Mo_Yr = replace(Outbreak_Mo_Yr, Outbreak_Mo_Yr == "0_00", 0))%>%
      select(-Month, -Yr)
    
  }
  return(empt.df)
}

#Identify the Peak populations of infected mosquitoes by MosqYear
InfectedMosqPeak <- function(df){
  dat = data.frame() #Blank dataframe
  j = 1 #Counter
  for(y in unique(df$MosqYear)){#For each MosqYear
    filt = filter(df, MosqYear == y)#Filter by MosqYear
    ind.iapeak = which.max(filt$IA)#which row has the peak infected aedes for that year
    time.ia <- filt$time[ind.iapeak]#What simulation day is that
    max.ia <- filt$IA[ind.iapeak]#What is the number of the peak
    
    ind.iaepeak <- which.max(filt$IAE) #which row has the peak infected aedes eggs for that year
    max.iae <- filt$IAE[ind.iaepeak]#What is the number of the peak
    
    ind.sapeak = which.max(filt$SA) #which row has the peak susceptible aedes for that year
    time.sa <- filt$time[ind.sapeak]#What simulation day is that
    max.sa <- filt$SA[ind.sapeak]#What is the number of the peak
    
    ind.icpeak = which.max(filt$IC) #which row has the peak infected culex for that year
    time.ic <- filt$time[ind.icpeak]#What simulation day is that
    max.ic <- filt$IC[ind.icpeak]#What is the number of the peak
    
    ind.scpeak = which.max(filt$SC) #which row has the peak susceptible culex for that year
    time.sc <- filt$time[ind.scpeak]#What simulation day is that
    max.sc <- filt$SC[ind.scpeak]#What is the number of the peak
    
    dat[j,1] <- y
    dat[j,2] <- time.ia
    dat[j,3] <- max.ia
    dat[j,4] <- time.ic
    dat[j,5] <- max.ic
    dat[j,6] <- time.ic - time.ia
    dat[j,7] <- max.ic - max.ia
    
    if(max.ic >max.ia){ #If have more infected culex
      dat[j,8] <- "More Infected Culex"
    }
    if(max.ia >max.ic){#If have more infected aedes
      dat[j,8] <- "More Infected Aedes"
    }
    if(max.ia == 0 & max.ic ==0){#If have zero infected mosquitoes
      dat[j,8] <- "No infected mosquitoes this year"
    }
    
    dat[j,9] <- max.iae
    dat[j,10] <- time.sa 
    dat[j,11] <- max.sa 
    dat[j,12] <- time.sc
    dat[j,13] <- max.sc
    dat[j,14] <- max.ic/max.ia
    
    
    j <- j+1
  }
  #Name columns
  names(dat) <- c("MosqYear", "SimDay_Peak_IAedes", "Peak_IAedes_Pop", "SimDay_Peak_ICulex", "Peak_ICulex_Pop", "Days_Delay_in_IPeak", "IC_minus_IA_Difference_In_IPeak_Pop", "Species_Higher_IPeak", "Peak_IAE", "SimDay_Peak_SAedes", "Peak_SAedes_Pop", "SimDay_Peak_SCulex", "Peak_SCulex_Pop", "Ratio_iCu_to_iAe")
  return(dat)
}


#Calculate mean vector populations
Get.Mean.Vec.Pops <- function(df, pop_var){

  #Filter for timesteps that have at least one mosquito present
  mosq_pops <- df%>%
    filter(UQ(sym(pop_var)) >=1)
  
  m_mosq_pop <- mean(unlist(mosq_pops[pop_var]))
  max_mosq_pops <- max(unlist(mosq_pops[pop_var]))
  
  #Max peak mosquito pop
  mosq_pops <- mosq_pops%>%
    group_by(MosqYear)%>%
    summarise(MYear = unique(MosqYear),
              PeakPop = max(UQ(sym(pop_var))))
  
  #Mean peaks
  #mean peak of mosquitoes per year
  m_peak_Mosq_pop <- mean(mosq_pops$PeakPop)
 return(c(m_mosq_pop, max_mosq_pops, m_peak_Mosq_pop)) 
}
  
#' Mean Annual proportion of recovered hosts
Get.Mean.SeroP.MosqY <- function(df){
  rs = df$RS
  rl = df$RL
  ns = df$NS
  nl = df$NL
  if(any(is.na(rs)) | any(is.na(rl)) | any(is.na(ns)) | any(is.na(nl))){
    return(paste("RS, RL, NS or NL was NA"))
  }else{
    dat = df%>%
      mutate(RL = replace(RL, RL < 1, 0))%>%
      mutate(RS = replace(RS, RS < 1, 0))%>%
      mutate(NS = replace(NS, NS < 1, 0))%>%
      mutate(NL = replace(NL, NL < 1, 0))%>%
      mutate(Prop_RSL = (RS + RL)/(NS + NL))%>%
      group_by(MosqYear)%>%
      summarise(max_PropRSL = max(Prop_RSL))
    Mean.SeroPrev = mean(dat$max_PropRSL)
    return(Mean.SeroPrev)
  }
}

#'  How long until the virus goes extinct (days)
Get.Extinct.Day <- function(df){
  is = df$IS
  il = df$IL
  if(any(is.na(is)) | any(is.na(il))){
    return(paste("IS or IL was NA"))
  }else{
    a = (df$IS >=1) | (df$IL >=1)
    a.true = (a == TRUE)
    LastDay = ifelse(any(a.true), tail(which(a.true), 1), 0)
    return(LastDay)
  }
}

#'  How long until the virus goes extinct (days)
Get.Extinct.IAE.Day <- function(df, min.pop){
  iae = df$IAE
  if(any(is.na(iae)) ){
    return(paste("IAE was NA"))
  }else{
    a = (df$IAE >=1) 
    a.true = (a == TRUE)
    LastDayAny = ifelse(any(a.true), tail(which(a.true), 1), 0)
    #Seems that the pop ~ <144000 cannot infect sheep
    b = (df$IAE >= min.pop)
    b.true = (b == TRUE)
    LastDay_14400 = ifelse(any(b.true), tail(which(b.true), 1), 0)
    
    LastDays <- c(LastDayAny, LastDay_14400)
    
    return(LastDays)
  }
}


#'* Maximum size of an outbreak (by season) 
Get.Max.Outbreak.Size.MosqYear <- function(df, sigma){
  is = df$IS
  il = df$IL
  if(any(is.na(is)) | any(is.na(il))){
    return(paste("IS or IL was NA"))
  }else{
    df.sum = df%>%
      group_by(MosqYear)%>% #Summarise by MosqYear
      mutate(IS_count = if_else(IS <1, 0, IS))%>%
      mutate(IL_count = if_else(IL <1, 0, IL))%>%
      summarise(Endemic = round(sum(IS_count, IL_count, na.rm = TRUE)/(1/sigma),3))
    
    ind = which.max(df.sum$Endemic)
    
    #bind everything into a named vector
    Largest.Outbreaks = df.sum$Endemic[ind]
    
  }
  return(Largest.Outbreaks)#return cbound vector
}

#Average size of seasonal outbreak
Get.Mean.Outbreak.Size.MosqYear <- function(df, sigma){
  is = df$IS
  il = df$IL
  if(any(is.na(is)) | any(is.na(il))){
    return(paste("IS or IL was NA"))
  }else{
    df.sum = df%>%
      group_by(MosqYear)%>% #Summarise by MosqYear
      mutate(IS_count = if_else(IS <1, 0, IS))%>%
      mutate(IL_count = if_else(IL <1, 0, IL))%>%
      summarise(All.Outbreak = round(sum(IS_count, IL_count, na.rm = TRUE)/(1/sigma),3))
    Mean.Outbreak.size = mean(df.sum$All.Outbreak)
  }
  return(Mean.Outbreak.size)#return cbound vector
}

#Average outbreak length per season
Get.Mean.Outbreak.lengths.MosqYear <- function(df){
  is = df$IS
  il = df$IL
  if(any(is.na(is))|any(is.na(il))){ #If there are NAs 
    return(paste("IS or IL was NA")) #return that it was NA
  }else{
    df.sum = df%>% #summarise
      mutate(DaywOutbreak = if_else(IS >=1 | IL >=1, 1, 0))%>%
      group_by(MosqYear)%>% #by MosqYear
      summarise(Full.length = sum(DaywOutbreak, na.rm = TRUE))
    Mean.Outbreak.Length = mean(df.sum$Full.length)
  }
  return(Mean.Outbreak.Length)
}

#Get the proportion of infected eggs at the end of the simulation
Get.Prop.Infected.Eggs <- function(df){
  iae = df$IAE
  sae = df$SAE
  if(any(is.na(iae)) | any(is.na(sae))){
    return(paste("IAE or SAE was NA"))
  }else{
    dat = df%>%
      group_by(MosqYear)%>%
      mutate(Prop_IAE = IAE/(IAE + SAE))
    
    mean.prop.i = mean(dat$Prop_IAE)
    max.prop.i = max(dat$Prop_IAE)
    min.prop.i = min(dat$Prop_IAE)
    end.prop.i = tail(dat$Prop_IAE,1)
    
    prop.i = cbind(mean.prop.i, max.prop.i, min.prop.i, end.prop.i)
    return(prop.i)
  }
}

#Get the proportion of the mosquito population that is infected (IA, IC, IAE etc.)
Get.Prop.Infected.Mosq <- function(df, i_pop){
  ia = df$IA
  ea = df$EA
  sa = df$SA
  ic = df$IC
  ec = df$EC
  sc = df$SC
  if(any(is.na(ia)) | any(is.na(sa)) | any(is.na(ea)) | any(is.na(sc))  | any(is.na(ec))| any(is.na(ic))){
    return(paste("IA, EA, SA, IC, EC or SC was NA"))
  }else{
    if(i_pop == "IA"){
      tot.pop <- "NAedes"
    }else{
      if(i_pop == "IAE"){
        df$NAE <- df$IAE + df$SAE
        tot.pop <- "NAE"
      }else{
        letts <- str_length(i_pop)
        gen.pop <- str_sub(i_pop, 2, letts)
        tot.pop <- paste0("N", gen.pop)
      }}
    
    #Make a new column for the ratio
    df$Ratio <- df[i_pop]/df[tot.pop]
    
    #only asses ratio when there is at least one mosquito of that species present
    dat <- df[!df[tot.pop]<1, ]
    
    dat$Ratio <- as.numeric(unlist(dat$Ratio))
    
    #Calculate estimates
    mean.prop = mean(dat$Ratio, na.rm = TRUE)
    range.prop.hi = range(dat$Ratio, na.rm = TRUE)[2]
    range.prop.low = range(dat$Ratio, na.rm = TRUE)[1]
    
    #Bind as one vector
    prop.i = cbind(mean.prop, range.prop.low, range.prop.hi) 
    return(prop.i)
  }
}

################################################################
################################################################
