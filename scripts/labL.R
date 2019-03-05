## Setting up and loading data
library(plm)
library(foreign)
library(readstata13)
library(dplyr)
library(tidyverse)
library(haven)
library(olsrr)
occhist08 <- read_dta("./data/occhist08.dta")


#Create female column using ifelse function
occhist08$female <- ifelse(occhist08$sex==2,1,0)

# Occupational averages
attach(occhist08) # Specify data the following commands are operating on
  table(sex)
  table(female)
# aggregate - create a data set of occupational averages of female
  occAgg <- aggregate(female, by=data.frame(occ2), mean, na.rm=TRUE)
  head(occAgg)

# Try this using dplyr and piping
  occAgg2 <- occhist08 %>%
      filter(!is.na(occ2) & !is.na(sex)) %>%
      group_by(occ2) %>%
      summarize(mean(female))
  head(occAgg2)
  
# Subset data frame
  a <- data.frame(occhist08$caseid, occhist08$occ, occhist08$occ2)
  head(a)
  
# keep if job = 1
  occhist08 <- subset(occhist08, subset=job==1)
  
# check the number of cases before merging
  nrow(occhist08)
  occhist08 <- merge(occhist08, occAgg, by = "occ2", all.x = TRUE)  
  nrow(occhist08)
  head(occhist08)  
  summary(occhist08$occ_fem)  
  table(occhist08$occ2[occhist08$occ2 < 10])  
  
  # The mean of occ_fem for sex = 1 (males)
  summary(subset(occhist08$occ_fem, subset=sex==1))  
  # The mean of occ_fem for sex = 1
  mean(occhist08$occ_fem, na.rm =TRUE)
  mean(occhist08$occ_fem[occhist08$occ2 < 10], na.rm=TRUE)  
  # Average occ_fem for males and females in the data set
  mean(occhist08$occ_fem[occhist08$sex==1], na.rm=TRUE)  
  mean(occhist08$occ_fem[occhist08$sex==2], na.rm=TRUE)
  
  # Make table of proportion female in the persons occupation by gender
    occhist08$sex <- factor(occhist08$sex,
                            levels = c(1,2),
                            labels = c("male", "female"))
    occhist08 %>%
        group_by(sex) %>%
        summarize(pctfem = mean(occ_fem, na.rm=TRUE), n = n())

    
# Regression models - cross sectional, FE, and RE
    # OLS using only 1988 data observations
    mod.ols1988 <- lm(lnwage ~ sex + occ_fem + hgc + hrswk + tenur + 
                       exp802,
                        data = occhist08[occhist08$year==1988,])
    ols_regress(mod.ols1988)
    
    # OLS for entire dataset
    mod.ols <- lm(lnwage ~ sex + occ_fem + hgc + hrswk + tenur + exp802,
                  data=occhist08)
    ols_regress(mod.ols)
    
    # Fixed effects
    mod.fe <- plm(lnwage ~ sex + occ_fem + hgc + hrswk + tenur + exp802,
                  data=occhist08, index=c("caseid", "year"), na.action=na.omit, model="within")
    summary(mod.fe)    

    # Random effects
    mod.re <- plm(lnwage ~ sex + occ_fem + hgc + hrswk + tenur + exp802,
                  data=occhist08, index=c("caseid", "year"), na.action=na.omit, model="random")
    summary(mod.re)
    
# Reshaping data: social mobility
    occhist_00_88 <- occhist08[occhist08$year==2000 | occhist08$year == 1988,]
    summary(occhist_00_88) 
    az <- occhist_00_88[c("caseid", "year", "pctile_cat")]

# Using data to study individual career histories
  rm(list=ls()) #start clean data
  occhist08 <- read_dta("./data/occhist08.dta")
  
  # find someone who worked in pest control (occ2 == 455)
  pest_control <- occhist08 %>%
    arrange(caseid, year) %>%
    group_by(caseid) %>%
    filter(any (occ2 == 455)) %>%
    select(caseid, year, sex, occ, occ2, wage_re)
  table(pest_control$caseid)
  # pick out case 1342 as an example
  pest_control[pest_control$caseid==1342,]  
  
  # Find out what registered nurses between ages 26-28 did later in life
  male_nurses2528 <- occhist08 %>%
    arrange(caseid, year) %>%
    group_by(caseid) %>%
    filter(any(occ2 == 95 & sex ==1 & age >25 & age<28)) %>%
    select(caseid, year, sex, occ, occ2, wage_re)
  table(male_nurses2528$caseid)  
  # select example case 1773
  male_nurses2528[male_nurses2528$caseid==1773,]
  
  #See career of anyone who has ever been a nurse and doctor
  # registered nurse = 95, doctor = 84
  doctor_nurse <- occhist08 %>%
    arrange(caseid, year)%>%
    group_by(caseid) %>%
    filter(any (occ2 ==95)) %>%
    filter(any (occ2 ==84)) %>%
    select(caseid, year, sex, occ, occ2, wage_re)
  table(doctor_nurse$caseid)  
  doctor_nurse[doctor_nurse$caseid==3117,]  
  