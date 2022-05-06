###Variable tables for all files
#' Author: Mindy Rostal
#' Date 5/3/2022
#' 
#' Purpose: To ensure the exact same variables and populations are used for every simulation and sensitivity analysis.
#' # See Table S4 for references.

set.seed(6242015)#Set seed so LHC is reproducible

#Climate Variables 
DaysCumRainA <- 7
DaysCumRainC <- 3
TempAC <- 20
MinRainA <- 38
MinRainC <- 2
CulexHatchDelay <- 7
DaysCanHatchA <- 9

#SIR parameters
sigma_sl <- round(1/4,3)                     # recovery rate
g <- round(1/182.5,3)                        # lamb maturation rate unit days-1.
muS <- round(0.0001388352 , 5)               # death rate + slaughter rate 
soldS <- round(0.4*.005,4)                   # Sheep that are sold
rhoS <- round((.2*(sigma_sl+muS))/(1-.2),3)  # RVF specific mortality fraction in adult sheep
bL <- round(1.58/365,3)                      # Births - dorper 1.58 lambs per year
buyL <- round(0.5080688,3)                   # From cohort sheep rates
muL <- round(0.0003984711,4)                 # lamb mortality rate. unit days-1.
rhoL <- round((.75*(sigma_sl+muL))/(1-.75),3)# RVFV specific mortality fraction for lambs 
Abort <- .9                                  # abortion fraction among pregnant ewes infected with RVF
vax.prop <- 0                                # The proportion of the flock you want to vaccinate
#Calculation: p = vax/(vax+muL+g); p(vax +muL + g) = vax;  pvax + p(muL + g) = vax; p(muL + g) = (1-p)vax; vax = p*(muL+g)/(1-p) 
vax <- vax.prop*(muL+g)/ (1-vax.prop)        #vax = p*(muL+g)/(1-p)
omega_MA <- round(1/120,3)                   # maternal antibodies lost at 4 months
NLmax <- 800                                 #Carrying capacity for lambs/sheep
EgAE <- round((1/3),3)                       # rate at which adult Aedes lays eggs
EgNAE <- round(((2075/30)/2),3)              # number of female eggs an adult Aedes lays at a time
muA <-  .07                                  # Aedes mortality. 
muAE <- round(1/(365*2),3)                   # assume eggs survive 2 yr
NAEmax <- 100000                             # Aedes carrying capacity
phiA <- .36                                  # fraction of Aedes larvae/pupae surviving 
bhA <- .23                                   # hatching rate 
waitA <- round(1/3,3)                        # it takes 7 days before mosquitoes take their first bite
biteA <- 0.12                                # Aedes bite rate
epsilon <- round(1/((8+16)/2),3)             # Extrinsic Incubation time 
Tasl <- 0.41                                 # transmission to Aedes from sheep
Tsla <- .16                                  # transmission to sheep/lamb from Aedes
q <- .7                                      # transovarial transmission
alphaAE <- round(1/30,3)                     # time it takes for the eggs to be desiccated enough to hatch
EgCE <- round(1/3,3)                         # Egg laying rate 
EgNCE <- round(236/2,3)                      # average number of eggs an adult Culex lays at once divided by two because 50% are male and they aren't included in the model
delta <- .8                                  # fraction of infected Culex that lay eggs
psiC <- .82                                  # the fraction of eggs that hatch in the lab
phiC <- round((.2344+.0562),3)               # fraction of Culex that survive larval/pupal stages
bhC <- round((1/1.5),3)                      # 1/hatching time = hatching rate.
waitC <- round(1/3,3)                        # it takes 7 days before Mosquitoes take their first bite. 
muC <-  round((1-((.94+.97+.90)/3)),3)       # daily Culex mortality rate (1-daily survival rates). 
muCE <-  round((((1/1.5)-(psiC*(1/1.5)))/psiC),3)# daily egg mortality rate.  
NCEmax <-  200000                            # Culex carrying capacity
Tcsl <-  .71                                 # transmission  to Culex from sheep/lamb
Tslc <-  .31                                 # transmission to sheep/lamb from Culex
biteC <- 0.035                               # Culex bite rate

#Ranges for Latin Hypercube Sensitivity Analysis
#Model factors
biteA.min <- biteA * 0.5 # Aedes bite rate
biteA.max <- biteA * 1.5
Tasl.min <- Tasl * 0.5   # transmission to Aedes from sheep
Tasl.max <- Tasl * 1.5
Tsla.min <- Tsla * 0.5   # transmission to sheep/lamb from Aedes
Tsla.max <- Tsla * 1.5
muAE.min <- muAE * 0.5   # Aedes egg mortality rate
muAE.max <- muAE * 1.5
q.min <- q * 0.5         # transovarial transmission
q.max <- 1
Tcsl.min <- Tcsl * 0.5   # transmission to Culex from sheep/lamb (no units)
Tcsl.max <- 1
Tslc.min <- Tslc * 0.5   # transmission to sheep/lamb from Culex (no units)
Tslc.max <- Tslc * 1.5
biteC.min <- biteC * 0.5 # bite rate
biteC.max <- biteC * 1.5
sigma_sl.min <- sigma_sl * 0.5# Recovery rate
sigma_sl.max <- sigma_sl * 1.5
muC.min <- muC * 0.5     # Culex mortality rate
muC.max <- muC * 1.5
muA.min <- muA * 0.5     # Aedes mortality rate
muA.max <- muA * 1.5
epsilon.min <- epsilon * 0.5 # Extrinsic Incubation time 
epsilon.max <- epsilon * 1.5
rhoL.min <- rhoL* 0.5    # Specific RVF lamb mortality rate
rhoL.max <- 1
rhoS.min <- rhoS * 0.5   # Specific RVF sheep mortality rate
rhoS.max <- rhoS * 1.5


#Set parameters for development rates per Rueda et al.

dev.params.set <- c(rh025a = .15460,
                    HAa = 33255.57,
                    THa = 301.67,
                    HHa = 50543.49,
                    #Culex entropy factors
                    rh025c = .21945,
                    HAc = 28049.98,
                    THc = 298.60,
                    HHc = 35362.18)

#Vector of all parameters to be used in the model
param_vec <- cbind( DaysCumRainA, DaysCumRainC, TempAC, MinRainA, MinRainC, CulexHatchDelay,  DaysCanHatchA, 
                   sigma_sl, g, muS, soldS, rhoS, bL, buyL, muL, rhoL, Abort, vax, vax.prop, omega_MA, NLmax, 
                   EgAE, EgNAE, muA, muAE, NAEmax, phiA, bhA, waitA, epsilon, biteA, Tasl, Tsla, q, alphaAE,
                   EgCE, EgNCE, delta, phiC, bhC, waitC, muC, muCE, NCEmax, Tcsl, Tslc, biteC) #DaysofCumRainEpidemic, #PrctRainyDays,FrequencyofRainDaysOver,

#Vector of all parameters to be used in the R0 analysis
R0params <- c( DaysCumRainA=DaysCumRainA, DaysCumRainC=DaysCumRainC, TempAC=TempAC, MinRainA=MinRainA, MinRainC=MinRainC, CulexHatchDelay=CulexHatchDelay,  DaysCanHatchA=DaysCanHatchA, 
              sigma_sl=sigma_sl, g=g, muS=muS, soldS=soldS, rhoS=rhoS, bL=bL, buyL=buyL, muL=muL, rhoL=rhoL, Abort=Abort, vax=vax, omega_MA=omega_MA, NLmax=NLmax,
              EgAE=EgAE, EgNAE=EgNAE, muA=muA, muAE=muAE, NAEmax=NAEmax, phiA=phiA, bhA=bhA, waitA=waitA, epsilon=epsilon, biteA=biteA, Tasl=Tasl, Tsla=Tsla, q=q, alphaAE=alphaAE,
              EgCE=EgCE, EgNCE=EgNCE, delta=delta, phiC=phiC, bhC=bhC, waitC=waitC, muC=muC, muCE=muCE, NCEmax=NCEmax, Tcsl=Tcsl, Tslc=Tslc, biteC=biteC)#, mdev_ALP=mdev_ALP, mmu_ALP=mmu_ALP, mdev_CLP=mdev_CLP, mmu_CLP=mmu_CLP), DaysofCumRainEpidemic=DaysofCumRainEpidemic, PrctRainyDays=PrctRainyDays, FrequencyofRainDaysOver=FrequencyofRainDaysOver,

#Latin hypercube sensitivity analysis multilevel analysis
if(SA == TRUE){
                        lhs<-maximinLHS(h,49)
                        params.matrix_trans <- cbind(DaysCumRainA, DaysCumRainC, TempAC, MinRainA, MinRainC,  DaysCanHatchA, CulexHatchDelay,
                                                     sigma_sl = lhs[, 10] * (sigma_sl.max - sigma_sl.min) + sigma_sl.min, 
                                                     g, muS, soldS, 
                                                     rhoS = lhs[, 14] * (rhoS.max - rhoS.min) + rhoS.min, 
                                                     bL, buyL, muL, 
                                                     rhoL = lhs[, 18] * (rhoL.max - rhoL.min) + rhoL.min, 
                                                     Abort, vax, omega_MA, NLmax, 
                                                     EgAE, EgNAE, 
                                                     muA = lhs[, 25] * (muA.max - muA.min) + muA.min, 
                                                     NAEmax, phiA, bhA, 
                                                     epsilon = lhs[, 29] * (epsilon.max - epsilon.min) + epsilon.min,
                                                     waitA,
                                                     biteA = lhs[, 31] * (biteA.max - biteA.min) + biteA.min,
                                                     Tasl = lhs[, 32] * (Tasl.max - Tasl.min) + Tasl.min,
                                                     Tsla = lhs[, 33] * (Tsla.max - Tsla.min) + Tsla.min,
                                                     q = lhs[, 34] * (q.max - q.min) + q.min,
                                                     alphaAE,
                                                     muAE = lhs[, 36] * (muAE.max - muAE.min) + muAE.min,
                                                     EgCE, EgNCE, delta, psiC, phiC, bhC, 
                                                     muC = lhs[, 44] * (muC.max - muC.min) + muC.min, 
                                                     muCE, NCEmax, waitC,
                                                     Tcsl = lhs[, 47] * (Tcsl.max - Tcsl.min) + Tcsl.min,
                                                     Tslc = lhs[, 48] * (Tslc.max - Tslc.min) + Tslc.min,
                                                     biteC = lhs[, 49] * (biteC.max - biteC.min) + biteC.min) 
                         }

# Define starting populations
#Sheep
pop.sizeS  <- 800 #
pop.sizeL  <- pop.sizeS *.25
initial.IS <- 0
initial.RS <- pop.sizeS *.3
initial.VS <- 0
initial.SS <- pop.sizeS - initial.IS - initial.RS - pop.sizeL- initial.VS
initial.NS <- initial.SS + initial.IS + initial.RS + initial.VS

#Lambs

initial.IL <- 0
initial.RL <- 0
initial.AL <- initial.RS *.25
initial.VL <- 0
initial.SL <- pop.sizeL - initial.IL - initial.RL - initial.AL - initial.VL
initial.NL <- initial.SL + initial.IL + initial.RL + initial.AL + initial.VL

#Aedes adults
pop.sizeA      <- 0
initial.IA     <- 0
initial.EA     <- 0
initial.SA     <- pop.sizeA - initial.IA - initial.EA
initial.NAedes <- initial.SA + initial.IA + initial.EA

#Young adult Aedes
pop.sizeSAY <- 0
pop.sizeIAY <- 0
initial.SAY <-pop.sizeSAY
initial.IAY <-pop.sizeIAY

#Aedes larval pupal stage
pop.sizeSALP <- 0
pop.sizeIALP <- 0
initial.SALP <-pop.sizeSALP
initial.IALP <-pop.sizeIALP

#Aedes hatchable eggs
pop.sizeAE  <- 18000000
initial.IAE <- pop.sizeAE * 0.003
initial.SAE <- pop.sizeAE - initial.IAE
initial.newSAE <- 0
initial.newIAE <- 0

# Adult Culex
pop.sizeC  <- 0
initial.EC <- 0
initial.IC <- 0
initial.SC <- pop.sizeC - initial.IC - initial.EC

#Young adult Culex
pop.sizeCY <- 0
initial.SCY <-pop.sizeCY

#Culex larval pupal stage
pop.sizeSCLP <- 0
initial.SCLP <-pop.sizeSCLP

#Culex eggs
pop.sizeCE  <- 1000 
initial.SCE <- pop.sizeCE

pop_vec <- initial.populations <- c(SS=initial.SS, IS=initial.IS, RS=initial.RS, VS = initial.VS, SL=initial.SL, IL=initial.IL, RL=initial.RL, AL=initial.AL,  VL = initial.VL ,SALP = initial.SALP, SAY = initial.SAY, SA=initial.SA, EA = initial.EA, IALP=initial.IALP, IAY = initial.IAY, IA=initial.IA, SAE=initial.SAE, IAE=initial.IAE, newSAE = initial.newSAE, newIAE = initial.newIAE, SC=initial.SC, EC = initial.EC, IC =initial.IC, SCY = initial.SCY, SCLP=initial.SCLP, SCE=initial.SCE)# it is really important to keep this in the order in which the differential equations are set up!!  #NewSAE=initial.NewSAE, NewIAE=initial.NewIAE,

#Remove the following variables so they are not in the global environment
rm(list = c("DaysCumRainA", "DaysCumRainC", 
     "TempAC", "MinRainA", "MinRainC", "CulexHatchDelay", "DaysCanHatchA", 
     "sigma_sl", "g", "muS", "soldS", "rhoS",  "bL", "muL", "rhoL", "Abort", "vax", "vax.prop", "omega_MA", "NLmax","buyL",
     "sigma_sl.min", "sigma_sl.max", "rhoS.min", "rhoS.max", "rhoL.min", "rhoL.max",
     "EgAE", "EgNAE", "muA", "muAE", "NAEmax", "phiA", "bhA", "waitA", 
     "epsilon", "biteA", "Tasl", "Tsla", "q", "alphaAE",
     "EgCE", "EgNCE", "delta", "psiC", "phiC", "bhC", 
     "waitC", "muC", "muCE", "NCEmax", "Tcsl", "Tslc", "biteC", 
     "epsilon.min", "epsilon.max", "muA.min", "muA.max", "biteA.min", "biteA.max", "Tasl.min", "Tasl.max", "Tsla.min", "Tsla.max", "q.min", 'q.max', "muAE.min", "muAE.max",
     "muC.min", "muC.max", "Tcsl.min", "Tcsl.max", "Tslc.min", "Tslc.max", "biteC.min", "biteC.max",
     "pop.sizeA", "pop.sizeAE", "pop.sizeIALP", "pop.sizeSALP", "pop.sizeIAY", "pop.sizeSAY", "pop.sizeC",
     "pop.sizeCE", "pop.sizeCY", "pop.sizeSCLP", "pop.sizeL", "pop.sizeS", 
     "initial.SS", "initial.IS", "initial.RS", "initial.VS", "initial.NS", 
     "initial.SL", "initial.IL", "initial.RL", "initial.AL", "initial.VL", "initial.NL", 
     "initial.SALP", "initial.SAY", "initial.SA", "initial.EA", "initial.IALP", 
     "initial.IAY", "initial.IA", "initial.SAE", "initial.IAE", "initial.newSAE", "initial.newIAE", "initial.NAedes", 
     "initial.SC", "initial.EC", "initial.IC", "initial.SCY", "initial.SCLP", "initial.SCE"))
