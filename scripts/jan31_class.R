## January 31 2019
## Kate Brandt
## SOCI 709

## Dummy variables and interaction terms - Lecture D

# load necessary packages
library(foreign)
library(dplyr)


#import data
morg = read.dta("./data/morg07_small_1.dta", convert.factors = F)

##recode race
# Create recode column
morg$re = morg$race

# recoding categories into dummy variables
morg$re[morg$race > 4] = 5
morg$re[morg$hisp == 1] = 6
morg$re_names = as.factor(morg$re)
morg$re_names = recode_factor(morg$re_names,
                              
                              `1` = 'white',
                              
                              `2` = 'black',
                              
                              `3` = 'am indian',
                              
                              `4` = 'asian',
                              
                              `5` = 'other',
                              
                              `6` = 'hispanic')


#set reference group to white
morg = within(morg, re_names <- relevel(re_names, ref = 'white'))


# Create model for RACE gaps, Excl:white
mod_1 <- lm(morg$wage_re~morg$re_names)
summary(mod_1)

## set referece grouo for SEX to MALE
morg$re2 = morg$sex
morg$re_sex = as.factor(morg$re2)
morg$re_sex = recode_factor(morg$re_sex, 
                            `1` = "female",
                            `2` = "male")
morg = within(morg, re_sex <- relevel(re_sex, ref = "female"))


# Create model for RACE and SEX, Excl: {white, male}
mod_2 <- lm(morg$wage_re ~ 
              morg$re_names + 
              morg$re_sex)
summary(mod_2)

# Create model for RACE, SEX, and RACExSEX interaction, Excl: {white, male}
mod_2b <- lm(morg$wage_re ~ 
              morg$re_names + 
              morg$re_sex + 
              morg$re_names:morg$re_sex)
summary(mod_2b)

# Create model for RACE, SEX, RACExSEX interaction, AGE, and AGE^2. Excl: {white, male} 
## Generate AGE^2 variable
morg$age2 = (morg$age)^2
mod_3 <- lm(morg$wage_re ~ 
              morg$re_names + 
              morg$re_sex +
              morg$age +
              morg$age2)
summary(mod_3)

# Create model for RACE, SEX, RACExSEX interaction, AGE, AGE^2, EDUCATION. Excl: {white, male} 
mod_4 <- lm(morg$wage_re ~ 
              morg$re_names + 
              morg$re_sex +
              morg$age +
              morg$age2 +
              morg$edyrs)
summary(mod_4)

# Create model which builds on mod_4 to add interaction term for RACExSEX
mod_5 <- lm(morg$wage_re ~ 
              morg$re_names + 
              morg$re_sex +
              morg$age +
              morg$age2 + 
              morg$edyrs + 
              morg$re_names:morg$re_sex)
summary(mod_5)