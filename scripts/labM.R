## KB
## March 11 2019

## Lab M
library(readstata13)
library(dplyr)
library(tidyverse)
library(lme4)
library(plm)
library(haven)
library(olsrr)

mlevelm <- read_dta("./data/multilevel_m.dta")
head(mlevelm)
## add constant "1" to run model
mlevelm$one <- 1

# a constant only linear model to see if it works
mod.lm <- lm(y~one, mlevelm)
summary(mod.lm)

# first run the RE model to show that this is the basis for the multilevel models
# this intercept only model wouldn’t run in plm
#mod.re <- lm(y ~ one, data=mlevelm, index= c("schid", "i"), model="random",
            # effect = "nested")
# the above model wouldn't run..,"empty model" error...let me know if you solve this
# move on to the lmer model below

# now run the intercept only RE model in xtmixed (lmer)
# model <- lmer(math ~ homework + (1 | schid), data=mlmdata)
mod.xtmixed1 <- lmer(y ~ (1 | schid),  data=mlevelm)
summary(mod.xtmixed1)
# Stata uses ML, R uses REML by default, so try it with REML off.
# see https://rpubs.com/rslbliss/r_mlm_ws
mod.xtmixed2 <- lmer(y ~ (1 | schid),  data=mlevelm, REML=FALSE )
summary(mod.xtmixed2)

library(merTools)
mod1_re <- REsim(mod.xtmixed1)
summary(mod1_re)
