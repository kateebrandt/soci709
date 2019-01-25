# SOCI 709 Homework C3.4
# Kate Brandt, January 15 2019

# set workspace for SOCI709
setwd("~/R/SOCI709/analysis")

# import necessary package to import .dta file
library(foreign)
library(tidyverse)
library(dplyr)

# import data and create variables
attend <-read.dta("data/attend.dta")


# (i)

i_funs <- c("min", "max", "mean")
attend %>% select(atndrte, priGPA, ACT) %>% summarise_all(funs_(i_funs))

  
# (ii)
## model: atndrte = B0 + b1(priGPA) + b2(ACT) + u

## set variables 
head(attend)
y <- (attend$atndrte)
x1 <- (attend$priGPA)
x2 <- (attend$ACT)

## Create equation
attend <-
  cbind.data.frame(attend,y,x1,x2)
head(attend)
mod_ii <- lm(y~x1+x2, data=attend)
summary(mod_ii)

# (iii)



#(iv)


#(v)

