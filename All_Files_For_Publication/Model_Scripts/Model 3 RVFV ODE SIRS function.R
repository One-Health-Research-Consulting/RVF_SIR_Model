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

ODE.RVFV.deterministic.SIRS <- function(t, pop_vec, param_vec, sigEndA, sigC, sigdevA, sigdevC,  end.t, ...){
  with(as.list(c(param_vec, pop_vec)), {
    
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
    
     
    #Define transmission terms
    # Transmission to sheep/lambs from infected Aedes: 
    betaSLA  <- (biteA * Tsla) #Tsla = thetaSLA
    
    # Transmission to susceptible Aedes from infected sheep/lambs
    betaASL <- (biteA/(NS+NL) * Tasl) #Tasl = thetaASL
    
    # Transmission to sheep/lambs from infected Culex 
    betaSLC <- (biteC * Tslc) #Tslc = thetaSLC
    
    # Transmission to susceptible Culex from an infected sheep/lamb 
    betaCSL <- (biteC/(NS+NL) * Tcsl) #Tcsl = thetaCSL
    

#' ODE Equations
    #Adult sheep
    dSS_dt <-   g * SL - soldS * SS - betaSLC * SS /  (NS + NL) * IC - betaSLA * SS * IA /  (NS + NL) - muS * SS #- vax * SS * vax_scheme.s
    
    dIS_dt <-   g * IL - soldS * IS + betaSLC * SS /  (NS + NL) * IC + betaSLA * SS * IA /  (NS + NL) - muS * IS  - sigma_sl * IS  - rhoS * IS
    
    dRS_dt <-   g * RL - soldS * RS - muS * RS + sigma_sl * IS
    
    dVS_dt <-   g * VL - soldS * VS - muS * VS # vax * SS* vax_scheme.s +
    
    #Lambs
    dSL_dt <-   bL *  (1 - (NS / NLmax)) * SS  + omega_MA * AL + buyL - g * SL - betaSLC * SL /  (NS + NL) * IC - betaSLA * SL * IA /  (NS + NL) - muL * SL - vax * SL #* vax_scheme.l
    
    dIL_dt <-   betaSLC * SL /  (NS + NL) * IC + betaSLA * SL * IA /  (NS + NL) - g * IL - muL * IL - sigma_sl * IL - rhoL * IL

    dRL_dt <-   sigma_sl * IL - g * RL - muL * RL

    #Maternal Immunity
    dAL_dt <- + bL* (1 - (NS / NLmax)) * (RS + ((1-Abort)*IS) + VS) - omega_MA * AL - muL * AL#
    
    #Vaccination
    dVL_dt <- vax * SL - g * VL - muL * VL # * vax_scheme.l 
    
    #To prevent erroneous mosquito infections from fractions of an infected sheep/lamb, if IL and/or IS is <1 the it should be considered as 0
        ISmosq <- ifelse(IS < 1, 0, IS)
        ILmosq <- ifelse(IL<1, 0, IL)
    
    #Aedes
    dSALP_dt <- bhA * EndA * SAE * (1 - (NALP / NAEmax)) - dev_ALP * SALP - muALP * SALP 
        
    dSAY_dt <-  dev_ALP * SALP - waitA * SAY - muA * SAY 
        
    dSA_dt <-   waitA * SAY - betaASL * SA * (ILmosq+ISmosq) - muA * SA 
    
    dEA_dt <-   betaASL * SA * (ILmosq+ISmosq) - epsilon * EA - muA * EA

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
    
    dnewSAE_dt <- EgAE * EgNAE * (SA + EA + ((1 - q) * IA)) - alphaAE * newSAE - muAE * newSAE 
      
    dnewIAE_dt <- EgAE * EgNAE * q * IA - alphaAE * newIAE - muAE * newIAE
    
    #Culex 
    dSCLP_dt <- bhC * HatchC * SCE * (1 - (SCLP / NCEmax)) - dev_CLP * SCLP - muCLP  * SCLP 
    
    dSCY_dt <-  dev_CLP * SCLP - waitC * SCY - muC * SCY
    
    dSC_dt <-   waitC * SCY - betaCSL * SC  * (ILmosq + ISmosq) - muC * SC

    dEC_dt <-   betaCSL * SC * (ILmosq + ISmosq) - epsilon * EC - muC * EC
        
    dIC_dt <-   epsilon * EC  - muC * IC

    dSCE_dt <-  EgCE * EgNCE * (SC + EC) + delta * EgCE * EgNCE * IC - bhC * HatchC * SCE - muCE * SCE 
    
    #Derivatives
    derivatives <- c(dSS_dt, dIS_dt, dRS_dt, dVS_dt,
                     dSL_dt, dIL_dt, dRL_dt, dAL_dt, dVL_dt,
                     dSALP_dt, dSAY_dt, dSA_dt, dEA_dt, dIALP_dt, dIAY_dt, dIA_dt,
                     dSAE_dt, dIAE_dt, dnewSAE_dt, dnewIAE_dt,
                     dSC_dt, dEC_dt, dIC_dt, dSCY_dt, dSCLP_dt,dSCE_dt)
    
    #Function Output
    list(derivatives, c(NS=NS, NL=NL, NALP = NALP, NAedes = NAedes, NC = NC, muCLP = muCLP, dev_CLP = dev_CLP, muALP=muALP, dev_ALP = dev_ALP, ISmosq = ISmosq, ILmosq = ILmosq, HatchC = HatchC, EndA=EndA,  betaSLC= betaSLC, betaCSL =betaCSL))
  })
}