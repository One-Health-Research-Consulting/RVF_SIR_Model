#' Title: Assessment of sensitivity analysis
#' Author: Mindy Rostal
#' Date: 5/3/2022
#' 
#' lst.R0.var
#' Purpose: To get a list of data frames, where each dataframe has the value of R0 for each population size as you vary the variables listed in var_vec.
#'  
#' @param empty.dat an empty database
#' @param empty.lst an empty list
#' @param both.pop a vector of populations for the hosts, Aedes and Culex populations
#' @param A.pop a vector of populations for the hosts and Aedes
#' @param C.pop a vector of populations for the hosts and Culex (e.g. mean_popn vs peak_popn)
#' @param R0params.func the optimized parameters used in the R0 calculation (from Model 1 - RVFV Optimized Parameters.R)
#' @param get.fun.1 the function to run, e.g. calc_R0 from Function 4 Calculate R0.R
#' @param var_vec the list of variables that will be evaluated)


lst.R0.var <- function(empty.dat, empty.lst, both.pop, A.pop, C.pop, R0params.func, get.fun.1, var_vec){
  
  for(v in var_vec){
    df <- empty.dat
    
    if(v == "NS" | v == "NL" | v == "Na" | v == "NC"){
      
      N <- both.pop[v]
      Ns <- seq(N/10, 10*N, length=100)#Sequence of sheep population levels
      
      both.pop_2 <- both.pop
      A.pop_2 <- A.pop
      C.pop_2 <- C.pop
      
      for (n in Ns) {
        both.pop_2[v] <- n #Set NS in each population
        A.pop_2[v] <- n
        C.pop_2[v] <- n
        
        R01 <- get.fun.1(R0params.func, both.pop_2) #Calulate R0 for the three mosquito levels
        R02 <- get.fun.1(R0params.func, A.pop_2)
        R03 <- get.fun.1(R0params.func, C.pop_2)
        
        df <- rbind(df, #bind the results into a dataframe for each sheep population
                    data.frame(Value = n,
                               R0_both = R01,
                               R0_A = R02,
                               R0_C = R03))
      }
    }else{
        
        if(str_detect(v, "mu")){
          N <- R0params.func[v]
          Ns <- seq(0.00001, 1, by = 0.02)#Sequence to vary variable
        }else{
          N <- R0params.func[v]
          Ns <- seq(0, 1, by=0.02)#Sequence to vary variable
        }
        R0params.func_2 <- R0params.func
        
        for (n in Ns) {
          R0params.func_2[v] <- n #Set variable in R0params
          
          R01 <- get.fun.1(R0params.func_2, both.pop) #Calulate R0 for the three mosquito levels
          R02 <- get.fun.1(R0params.func_2, A.pop)
          R03 <- get.fun.1(R0params.func_2, C.pop)
          
          df <- rbind(df, #bind the results into a dataframe for each sheep population
                      data.frame(Value = n,
                                 R0_both = R01,
                                 R0_A = R02,
                                 R0_C = R03))
        }
      }
    df$Variable <- v
    empty.lst[[v]] <- df
  }
  return(empty.lst)
}
  
