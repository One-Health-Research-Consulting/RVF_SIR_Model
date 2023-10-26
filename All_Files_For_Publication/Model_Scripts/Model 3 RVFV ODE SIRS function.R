#' ---
#' title: "ODE RVFV SIRS Equations and Function"
#' author: "Mindy Rostal"
#' date: "May 3, 2022"
#' ---
#' 
#' 
#' Libraries
library(zoo)
library(dplyr)
library(codetools)

#' Parameters needed:
#' ==================
#' ODE.RVFV.deterministic.SIRS: t, pop_vec, param_vec

#' Function ODE.RVFV.deterministic.SIRS
#' ======================

ODE.RVFV.deterministic.SIRS <- function(t, pop_vec, param_vec, sigEndA, sigC, sigdevA, sigdevC, sigvax, vax.b, end.t, ...){
  with(as.list(c(param_vec, pop_vec)), {#function returns a list 
    
    #Populations
    NS <- SS + IS + RS + VS
    NL <- SL + IL + RL + AL + VL
    NAedes <- SA + EA + IA
    NAY <- SAY + IAY
    NALP <- SALP + IALP
    NAE <- SAE + IAE + newSAE + newIAE
    NC <- SC + EC + IC
    NAY <- SAY + IAY
    NCE<- SCE 
    TSAE <- SAE + newSAE
    TIAE <- IAE + newIAE
    
    #Hatching for each timestep
    #Aedes
    EndA <- sigEndA(t)
    
    #HCulex
    HatchC<- sigC(t)
    
    #Daily larval/pupal development and mortality rates - vary each day depending on temperature
    dev_CLP<- sigdevA(t) 
    dev_ALP<- sigdevC(t)
    muALP <- dev_ALP*((1-phiA)/phiA)
    muCLP <- dev_CLP*((1-phiC)/phiC)
    
    #Daily vaccination rates, only vaccinate adult sheep in burst situation
    if(vax.b == TRUE){
    vax_scheme.s <- sigvax(t) 
    vax_scheme.l <- sigvax(t)
    }else{
      vax_scheme.s <- 0
      vax_scheme.l <- sigvax(t)
    }
     
    #Define transmission terms
    # Transmission to sheep/lambs from infected Aedes: 
    TransmissionSLA  <- (biteA * Tsla) 
    
    # Transmission to susceptible Aedes from infected sheep/lambs
    TransmissionASL <- (biteA/(NS+NL) * Tasl) 
    
    # Transmission to sheep/lambs from infected Culex 
    TransmissionSLC <- (biteC * Tslc)
    
    # Transmission to susceptible Culex from an infected sheep/lamb 
    TransmissionCSL <- (biteC/(NS+NL) * Tcsl)
    

#' ODE Equations
    #Adult sheep
    dSS_dt <-   g * SL - soldS * SS - TransmissionSLC * SS /  (NS + NL) * IC - TransmissionSLA * SS * IA /  (NS + NL) - muS * SS - vax * SS * vax_scheme.s
    
    dIS_dt <-   g * IL - soldS * IS + TransmissionSLC * SS /  (NS + NL) * IC + TransmissionSLA * SS * IA /  (NS + NL) - muS * IS  - sigma_sl * IS  - rhoS * IS
    
    dRS_dt <-   g * RL - soldS * RS - muS * RS + sigma_sl * IS
    
    dVS_dt <-   vax * SS * vax_scheme.s + g * VL - soldS * VS - muS * VS
    
    #Lambs
    dSL_dt <-   bL *  (1 - (NS / NLmax)) * SS  + omega_MA * AL + buyL - g * SL - TransmissionSLC * SL /  (NS + NL) * IC - TransmissionSLA * SL * IA /  (NS + NL) - muL * SL - vax * SL * vax_scheme.l#
    
    dIL_dt <-   TransmissionSLC * SL /  (NS + NL) * IC + TransmissionSLA * SL * IA /  (NS + NL) - g * IL - muL * IL - sigma_sl * IL - rhoL * IL

    dRL_dt <-   sigma_sl * IL - g * RL - muL * RL

    #Maternal Immunity
    dAL_dt <- + bL* (1 - (NS / NLmax)) * (RS + ((1-Abort)*IS) + VS) - omega_MA * AL - muL * AL#
    
    #Vaccination
    dVL_dt <- vax * SL * vax_scheme.l - g * VL - muL * VL
    
    #To prevent erroneous mosquito infections from fractions of an infected sheep/lamb, if IL and/or IS is <1 the it should be considered as 0
        ISmosq <- ifelse(IS < 1, 0, IS)
        ILmosq <- ifelse(IL<1, 0, IL)
    
    #Aedes
    dSALP_dt <- bhA * EndA * SAE * (1 - (NALP / NAEmax)) - dev_ALP * SALP - muALP * SALP 
        
    dSAY_dt <-  dev_ALP * SALP - waitA * SAY - muA * SAY 
        
    dSA_dt <-   waitA * SAY - TransmissionASL * SA * (ILmosq+ISmosq) - muA * SA 
    
    dEA_dt <-   TransmissionASL * SA * (ILmosq+ISmosq) - epsilon * EA - muA * EA

    dIALP_dt <- bhA  * EndA * IAE * (1 - (NALP / NAEmax)) - dev_ALP * IALP - muALP * IALP 
    
    dIAY_dt <-  dev_ALP * IALP - waitA * IAY - muA * IAY 
         
    dIA_dt <-   waitA * IAY + epsilon * EA - muA * IA
    
    #Aedes Eggs
    SAEggHatch <- - bhA * EndA * SAE 
    SAEggLay   <- EgAE * EgNAE * q * SA
    SAEggDie   <- - muAE * SAE
    
    IAEggHatch <- - bhA * EndA * IAE 
    IAEggLay   <- EgAE * EgNAE * q * IA
    IAEggDie   <- - muAE * IAE
    
    dSAE_dt <-  alphaAE * newSAE - bhA * EndA * SAE - muAE * SAE 
    
    dIAE_dt <-  alphaAE * newIAE - bhA * EndA * IAE - muAE * IAE 
    
    dnewSAE_dt <- EgAE * EgNAE * (SA + ((1 - q) * IA)) - alphaAE * newSAE - muAE * newSAE 
      
    dnewIAE_dt <- EgAE * EgNAE * q * IA - alphaAE * newIAE - muAE * newIAE
    
    #Culex 
    dSCLP_dt <- bhC * HatchC * SCE * (1 - (SCLP / NCEmax)) - dev_CLP * SCLP - muCLP  * SCLP 
    
    dSCY_dt <-  dev_CLP * SCLP - waitC * SCY - muC * SCY
    
    dSC_dt <-   waitC * SCY - TransmissionCSL * SC  * (ILmosq + ISmosq) - muC * SC

    dEC_dt <-   TransmissionCSL * SC * (ILmosq + ISmosq) - epsilon * EC - muC * EC
        
    dIC_dt <-   epsilon * EC  - muC * IC

    dSCE_dt <-  EgCE * EgNCE * SC + delta * EgCE * EgNCE * IC - bhC * HatchC * SCE - muCE * SCE 
    
    #Derivatives
    derivatives <- c(dSS_dt, dIS_dt, dRS_dt, dVS_dt,
                     dSL_dt, dIL_dt, dRL_dt, dAL_dt, dVL_dt,
                     dSALP_dt, dSAY_dt, dSA_dt, dEA_dt, dIALP_dt, dIAY_dt, dIA_dt,
                     dSAE_dt, dIAE_dt, dnewSAE_dt, dnewIAE_dt,
                     dSC_dt, dEC_dt, dIC_dt, dSCY_dt, dSCLP_dt,dSCE_dt)
    
    #Function Output
    list(derivatives, c(NS=NS, NL=NL, NALP = NALP, NAedes = NAedes, NC = NC, muCLP = muCLP, dev_CLP = dev_CLP, muALP=muALP, dev_ALP = dev_ALP, ISmosq = ISmosq, ILmosq = ILmosq, HatchC = HatchC, EndA=EndA,  TransmissionSLC= TransmissionSLC, TransmissionCSL =TransmissionCSL))
  })
}