## ---------------------------------------------------------------------- ##
## funcs.R -------------------------------------------------------------- ##
## Author: Peter Norwood, NCSU ------------------------------------------ ##
## Purpose: general functions used for many other scripts --------------- ##
## ---------------------------------------------------------------------- ##

## set working directory 
setwd("C:/Users/peter/Dropbox/SMART Thompson Sampling/SMART_Thompson")

## load libraries
library(tidyverse)
library(MASS,exclude="select")

## random_X1 
## Purpose: generate response after the first step
## param alpha: mean vector for alpha0 + alpha1*X0 + alpha2*I(A_1=1)
## param X0: baseline pain score
## param A1: first-stage treatment
## return X1: response at stage one
random_X1 <- function(alpha,X0,A1,sigma_X1){
  mean_vec <- alpha[1] + alpha[2]*X0 + alpha[3]*A1
  X1 <- rnorm(1,mean_vec,sigma_X1)
  return(X1)
}

## vectorize random_X1
random_X1 <- Vectorize(random_X1,vectorize.args = c("X0","A1"))

## random_Y
## Purpose: generate outcome (Y) from the SMART
## param beta: mean vector
## param X0: baseline pain score
## param A1: first-stage treatment
## param X1: first-stage response
## param A2: second-stage treatment
## param sigma_Y: residual error eps ~ N(0,sigma^2)
## return Y: random outcome
random_Y <- function(beta,X0,A1,X1,A2,sigma_Y){
  
  mean_vec <- beta[1] + 
              beta[2]*X0
              beta[3]*as.numeric(A1==1) +
              beta[4]*X1 +
              beta[5]*as.numeric(A2==0) + 
              beta[6]*as.numeric(A2==2) +
              beta[7]*as.numeric(A2==3) +
              beta[8]*as.numeric(A2==4) +
              beta[9]*as.numeric(A2==5)
  
  Y <- rnorm(1,mean_vec,sigma_Y)
  return(Y)
  
}

## vectorize outcome
outcome <- Vectorize(random_Y,vectorize.args = c("X0","A1","X1","A2"))


## treat_A1 
## Purpose: give treatment A1 consistent with regimes
## param regime: regime to base treatment on
## return A1: treatment
treat_A1 <- function(regime){
  
  if(regime %in% c(1,2,3,4)){
    A1 <- 0
  }else{
    A1 <- 1
  }
  return(A1)
}

## treat_A2
## Purpose: give treatment A2 consistent with regimes
## param regime: regime to base treatment on
## param R: response at stage one
## return A2: treatment
treat_A2 <- function(regime,R){
  
  ## regime 1
  if(regime==1){
    
    if(R==1){A2 <- 2}
    else{A2 <- 4}
  
  ## regime 2
  }else if(regime==2){
    
    A2 <- 2
  
  ## regime 3
  }else if(regime==3){
    
    if(R==1){A2 <- 3}
    else{A2 <- 4}
  
  ## regime 4 
  }else if(regime==4){
    
    if(R==1){A2 <- 3}
    else{A2 <- 2}
    
  ## regime 5
  }else if(regime==6){
    
    if(R==1){A2 <- 3}
    else{A2 <- 0}
    
  ## regime 6
  }else if(regime==6){
    
    A2 <- 5
    
  ## regime 7
  }else if(regime==7){
    
    if(R==1){A2 <- 3}
    else{A2 <- 0}
    
  ## regime 8
  }else{
    
    if(R==1){A2 <- 3}
    else{A2 <- 5}
  
  }
  
  return(A2)
}
