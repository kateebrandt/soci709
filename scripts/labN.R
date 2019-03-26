## KB Mar 25 2019
## Lab N - SOCI 709
 
# R code
# the data used here is in the Lecture N dropbox folder


rm(list=ls())
library(readstata13)
library(dplyr)
library(tidyverse)
library(lme4)
library(plm)

getwd()


hsb2 <- read.dta13("./data/hsb2.dta")
save(hsb2, file="hsb2.RData")

head(hsb2)

# m1,  a constant only linear model (to see if it works)
mod.m1 <- lm(mathach ~1  , data=hsb2 )
summary(mod.m1)


# m2
mod.m2 <- lmer(mathach ~ (1 | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m2)

# m3
mod.m3 <- lmer(mathach ~ ses+ (1 | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m3)

# m4
mod.m4 <- lmer(mathach ~ ses + (1 + ses | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m4)

# m4b: note that this modedl differs from model m4 becasuse
# in m4b to covariance between the random intercept and the random coefficient for
# ses is set to 0, whil in m4 they are allowed to be correlated (see the corresponding models in Stata).

mod.m4b <- lmer(mathach ~ ses + (1 | schoolid) + (0+ ses | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m4b)

# m5
mod.m5 <- lmer(mathach ~ ses + female+ (1 + ses | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m5)

# m6
mod.m6 <- lmer(mathach ~ ses + female + avg_ses + (1 + ses | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m6)



hsb2$c_ses <- hsb2$ses*hsb2$avg_ses

hsb2 %>%
  filter(!is.na(c_ses)) %>%
  summarize(mean(c_ses), min(c_ses), max(c_ses), n=n())

hsb2 %>%
  slice(which.max(c_ses))

# m7
mod.m7 <- lmer(mathach ~ ses + female + avg_ses + c_ses + (1 + ses | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m7)

# m8
hsb2$c_female <- hsb2$avg_ses*hsb2$female
# note m8a tests adding the random coefficient for female first.  The random coefficient s.e. are different for female compared to Stata
mod.m8a <- lmer(mathach ~ ses + female + (1 + female | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m8a)

mod.m8 <- lmer(mathach ~ ses + female + avg_ses + c_female + (1 + ses + female | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m8)

# m9 …didn’t converge with REML=FALSE
hsb2$ses_female <- hsb2$ses*hsb2$female
mod.m9 <- lmer(mathach ~ ses + female + avg_ses + c_female + ses_female + (1 + ses + female | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m9)



# try it by removing the REML=FALSE

mod.m9 <- lmer(mathach ~ ses + female + avg_ses + c_female + ses_female + (1 + ses + female | schoolid),  data=hsb2)

summary(mod.m9)





# note: this didn't converge in R...a "singular fit"...need to alter the model restricting the RE covariances

mod.m9 <- lmer(mathach ~ ses + female +   + avg_ses + c_female + ses_female + (1  | schoolid) + (0 + ses  | schoolid) +
                 (0 + female | schoolid),  data=hsb2, REML=FALSE)

summary(mod.m9)




# m10
table(hsb2$sector)
hsb2$cath_female <- hsb2$sector*hsb2$female
mod.m10 <- lmer(mathach ~ ses + female + avg_ses + sector + cath_female + (1 + ses + female | schoolid),  data=hsb2, REML=FALSE)
summary(mod.m10)
mod.m10 <- lmer(mathach ~ ses + female + avg_ses + sector + cath_female + (1 | schoolid) + (0 + ses | schoolid) + 
                  (0 + female | schoolid), REML = FALSE, data=hsb2)
summary(mod.m10)

# m11
mod.m11 <- lm(mathach ~ ses + female + avg_ses + sector + cath_female ,  data=hsb2)
summary(mod.m11)

# growth curve example
asian <- read.dta13("./data/asian.dta")
save(asian, file="asian.RData")
head(asian)



asian$age2 <- asian$age^2

asian$girl <- ifelse(asian$gender==2,1,0)



mod.growth1 <- lmer(weight ~ age + age2 + (1 | id), data=asian)

summary(mod.growth1)



mod.growth2 <- lmer(weight ~ age + age2 + girl + (1 | id), data=asian)

summary(mod.growth2)



mod.growth3 <- lmer(weight ~ age + age2 + girl + (1 + girl | id), data=asian)

summary(mod.growth3)



asian$girl_age <- asian$girl*asian$age



mod.growth4 <- lmer(weight ~ age + age2 + girl + girl_age + (1 + girl | id), data=asian)

summary(mod.growth4)









# see

# https://datascienceplus.com/analysing-longitudinal-data-multilevel-growth-models-i/

