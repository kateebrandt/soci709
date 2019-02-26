# KB
# February 19, 2019
# SOCI709 Lab H

library(olsrr)

# PART A - Heteroskedasticity
## Breusch-Pagan test
## Step (1) Create the data

x = runif(5000)*10
z = runif(5000)*12
w = runif(5000)*5

## Create the e from x
e = rnorm(5000)*x

## Generate y
y = 5+3*x+3*z+4*w+10*e

## Combine to data frame then regress
a_df <- data.frame(y,x,z,w,e)
mod_a <- lm(y~x+z+w, a_df)
ols_regress(mod_a)

## Step (2) Breusch-Pagan test
ols_test_breusch_pagan(mod_a, rhs = TRUE, multiple = TRUE)

## Step (3) Huber-White sandwich estimator/robustness test
library(foreign)
library(sandwich)
library(lmtest)

# Estimate standard errors with robust regression
coeftest(mod_a, vcov = vcovHC(mod_a, "HC1"))


# PART B - Multicollinearity
# Correlation matrix
cor(a_df[,1:4])
# VIF estimate
ols_vif_tol(mod_a)

# PART C - Outliers and Influential cases


