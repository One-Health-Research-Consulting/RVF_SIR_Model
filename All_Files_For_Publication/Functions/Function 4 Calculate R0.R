#' Title: Calculate and Plot R0
#' Author: Mindy Rostal,
#' Date: 5/3/2022
#' 
#' 
#' Purpose: To calculate R0 (or effective R0) at different population sizes, produce Figures S6, S7 and S8 and parts of Figures 1 and S9. Figure S7 is produced using the mean peak vector populations. 

#Libraries 
library(dplyr)
library(ggplot2)
library(ggtext)

theme_louise <- function () { 
  theme_grey(base_size=14, base_family="Avenir") 
}



calc_R0 <- function(parameters, pop_size, DEBUG=FALSE) {
  with(as.list(c(parameters, pop_size)), {
    
    # These weren't in parameters
    muALP <- mdev_ALP * (1 - phiA) / phiA
    muCLP <- mdev_CLP * (1 - phiC) / phiC
    
    # Set betas
    beta_SLA <- Tsla * biteA / (NS + NL)
    beta_ASL <- Tasl * biteA / (NS + NL)
    beta_SLC <- Tslc * biteC / (NS + NL)
    beta_CSL <- Tcsl * biteC / (NS + NL)
    
    # Proportions at equilibrium (calculated in Maxima)
    pSS <- (muS + g) / (muS + vax + g)
    pSL <- pSS
    pSA <- (bhA * mdev_ALP * waitA * alphaAE) / ((muA + waitA) * (muA * muAE * muALP + alphaAE * muA * muALP + bhA * muA * muALP + mdev_ALP * muA * muAE + mdev_ALP * alphaAE * muA + bhA * alphaAE * muA + bhA * mdev_ALP * muA + bhA * mdev_ALP * alphaAE))
    pSC <- (bhC * mdev_CLP * waitC) / ((muC + waitC) * (muC * muCLP + mdev_CLP * muC + bhC * muC + bhC * mdev_CLP))
    
    # Constant birth rate (calculated in Maxima)
    adjusted_birth_rate <- (muA * (muA + waitA) * (muAE + bhA) * (muAE + alphaAE) * (muALP + mdev_ALP)) / (bhA * mdev_ALP * waitA * alphaAE)
    
    if (DEBUG) {
      cat("pSS, pSL = ", pSS, ", pSA = ", pSA, ", pSC = ", pSC,
          ", ABR = ", adjusted_birth_rate, "\n", sep="")
    }
    
    
    compartments <- c("IS", "IL", "EA", "newIAE", "IAE", "IALP", "IAY", "IA", "EC", "IC")
    at_infection <- compartments[c(1,2,3,4,9)]
    Tr <- Sigma <- matrix(0, nrow=10, ncol=10)
    rownames(Tr) <- colnames(Tr) <- rownames(Sigma) <- colnames(Sigma) <- compartments
    
  
    # New Transmissions are IS, IL, EA, newIAE, and EC
    #  1 dIS: ... + β_SLC*(p_SS*NS)*IC + β_SLA*(p_SS*NS)*IA
    Tr["IS", "IA"] <- + beta_SLA * pSS * NS
    Tr["IS", "IC"] <- + beta_SLC * pSS * NS
    #  2 dIL: ... + β_SLC*(p_SL*NL)*IC + β_SLA*(p_SL*NL)*IA
    Tr["IL", "IA"] <- + beta_SLA * pSL * NL
    Tr["IL", "IC"] <- + beta_SLC * pSL * NL
    #  3 dEA: + β_ASL*(p_SA*NA)*(IL + IS) - ...
    Tr["EA", "IS"] <- + beta_ASL * pSA * (Na/pSA) #fixed
    Tr["EA", "IL"] <- + beta_ASL * pSA * (Na/pSA) #fixed
    #  4 dnewIAE: + q*adjusted_birth_A*IA - ...
    Tr["newIAE", "IA"] <- + q * adjusted_birth_rate
    #  9 dEC: + β_CSL*(p_SC*NC)*(IL + IS) - ...
    Tr["EC", "IS"] <- + beta_CSL * pSC * (NC/pSC) #fixed
    Tr["EC", "IL"] <- + beta_CSL * pSC * (NC/pSC) #fixed
    
    # Transitions
    #  1 dIS: - σ*IS - μ_S*IS - ρ_S*IS + g*IL - sold_S*IS + ...
    Sigma["IS", "IS"] <- -sigma_sl - g - muS - rhoS - soldS
    Sigma["IS", "IL"] <- + g
    #  2 dIL: - σ*IL - μ_L*IL - ρ_L*IL - g*IL + ...
    Sigma["IL", "IL"] <- - sigma_sl - muL - rhoL - g
    #  3 dEA: + ... - ε*EA - μ_A*EA
    Sigma["EA", "EA"] <- - epsilon - muA
    #  4 dnewIAE: ... - α_AE*newIAE - μ_AE*newIAE
    Sigma["newIAE", "newIAE"] <- - alphaAE - muAE
    #  5 dIAE: + α_AE*newIAE - bh_A*IAE - μ_AE*IAE
    Sigma["IAE", "newIAE"] <- + alphaAE
    Sigma["IAE", "IAE"] <- - bhA - muAE
    #  6 dIALP: + bh_A*IAE - dev_ALP*IALP - μ_ALP*IALP
    Sigma["IALP", "IAE"] <- + bhA
    Sigma["IALP", "IALP"] <- - mdev_ALP - muALP
    #  7 dIAY: + dev_ALP*IALP - wait_A*IAY - μ_A*IAY
    Sigma["IAY", "IALP"] <- + mdev_ALP
    Sigma["IAY", "IAY"] <- - waitA - muA
    #  8 dIA: + wait_A*IAY - μ_A*IA + ε*EA
    Sigma["IA", "EA"] <- + epsilon
    Sigma["IA", "IAY"] <- + waitA
    Sigma["IA", "IA"] <- - muA
    #  9 dEC: ... - ε*EC - μ_C*EC
    Sigma["EC", "EC"] <- - epsilon - muC
    # 10 dIC: + ε*EC - μ_C*IC
    Sigma["IC", "EC"] <- + epsilon
    Sigma["IC", "IC"] <- - muC
    
    KL <- -Tr %*% solve(Sigma)
    K <- KL[at_infection, at_infection]
    evals <- max(Re(eigen(K)$values))
    # evals <- Re(eigen(KL)$values)
    R0 <- max(evals)
    
    if (DEBUG) {
      print("Transmissions:")
      print(Tr)
      print("Transitions:")
      print(Sigma)
      print("NGM with large domain:")
      print(KL)
      print("NGM with small domain:")
      print(K)
    }
    
    return(R0)
  })
}

#Function to calculate the effective R0 for each row in the final.populations dataframe
R_eff <- function(pop_matrix, params, get.fun1){
  #name the objects in the vector made from the row of the data.frame
  name_pops = c(NS = pop_matrix[[1]], NL = pop_matrix[[2]], Na = pop_matrix[[3]], NC = pop_matrix[[4]])
  
  #Run each row of populations with the R0params
  R0 = get.fun1(params, name_pops)
  return(R0)
}
