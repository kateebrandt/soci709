# lecture 3 matrices
# This is the wrong order of elements to create the correct matrix, pay attn to order
#a <- c(1,1,3,1,2,5,1,3,8) 


library(tidyverse)

##Lab Assignment
# create y 
y <- c(15,17,18,16,20,21)
dim(y) <- c(6,1)
y

#create x
#start by creating vectors

gender<- c(0,0,0,1,1,1)
occupation <- c(1,1,0,1,0,0)
constant <- c(1,1,1,1,1,1)

x <- c(gender, occupation, constant)
dim(x) <- c(6, 3)

x


#create transpose of x
xtranp <- t(x)
xtranp

#create denominator xx
xx <- xtranp%*%x
xx

# create inverse of denom xx
xxinv <- solve(xx)
xxinv

# create xy
xy <- xtranp%*%y
xy

#create beta matrix
beta <- xxinv%*%xy
beta

#check your work using regression
mod_lab_a3 <- lm(y~gender+occupation)
summary(mod_lab_a3)



