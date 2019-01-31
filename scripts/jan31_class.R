## January 31 2019
## Kate Brandt
## SOCI 709

## Dummy variables and interaction terms - Lecture D


library(foreign)

#requires dplyr to run below. It's a really good package, so I recommend installing this using: install.packages("dplyr")
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


# Create model
mod_lab6_exclwhite <- lm(morg$wage_re~morg$re_names+morg$sex+morg$age + morg$re_names:morg$sex)
summary(mod_lab6_exclwhite)
