# KB
# February 12, 2019
# Class - SOCI709

## Selecting random value from randomly distributed data
library(dplyr)
library(stats)
library(base)
estols2 <- function(n, xmean=0, x_sd=3, emean=0, e_sd=15, xcoef=3){
  
  # The following steps are based on Lab E STATA code
  # Create data for x, y, and e
  
  # Create sample x [from STATA: "gen x=3*invnorm(uniform())"]
  #(standard deviation = 3, mean = 0)
  x <- rnorm(n, mean = xmean, sd = x_sd)
  
  #Create sample e [from STATA: "gen e=15*invnorm(uniform())"
  e <- rnorm(n, mean = emean, sd = e_sd)
  
  #Create sample y
  y <- xcoef*x+e
  
  #create data frame to regress
  df <- data.frame(y,x,e)
  samp <- df[sample(nrow(df), 5), ]
  
  #output regression
  model <- (lm(y~x, data = df))
  coeflist <- model$coefficients[2] %>% unname
  
  return(list("beta coefficient" = coeflist, "sample of data" = samp))
  
}

###########################################################################
# Lecture on hetero skedasticity (continued, Feb 12)

# Use this link for VIF and other analysis of regression:
# [link](https://datascienceplus.com/multicollinearity-in-r/)



