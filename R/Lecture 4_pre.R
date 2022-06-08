## -------------------------------------------- ##
## EDSD 2021-2022: Population projections
## Lecture 4
## Extensions of Matrix Projections
## Date: 08/06/2022
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##


##----- EXERCISE 1: RC migration schedule --------
rm(list=ls(all=TRUE))

## load the migest package
library(migest)
library(tidyverse)

## check the fundamental RC parameters
rc_model_fund
my.pars <- rc_model_fund %>%
  select(value) %>% pull()

## function to construct RC schedule
mxRG <- function(x,pars){
  t1 <- pars[1]*exp(-pars[2]*x)
  t2 <- pars[3]*exp(-pars[4]*(x-pars[5])-exp(-pars[6]*(x-pars[5])))
  mx <- t1+t2+pars[7]
  mx <- mx/sum(mx)
  return(mx)
}
## five-year age groups (works well also with one-year)
x <- seq(0,85,5)
mx <- mxRG(x=x,pars=my.pars)
plot(x, mx, type="o",pch=16)
## assume a total of 100000 net migration counts
I <- 1e5
Ix <- I*mx
sum(Ix)
plot(x, Ix, type="o",pch=16,
     xlab = "Age group",ylab= "Net migrant counts",
     main="RC migration schedule for 100,000 net migrants")



##----- EXERCISE 2: Projections with open population --------
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(viridis)
library(migest)

## set working directory in active folder (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading the data 
dir()
load("data/EDSD.lecture3.Rdata")  

## function to construct RC schedule
mxRG <- function(x,pars){
  t1 <- pars[1]*exp(-pars[2]*x)
  t2 <- pars[3]*exp(-pars[4]*(x-pars[5])-exp(-pars[6]*(x-pars[5])))
  mx <- t1+t2+pars[7]
  mx <- mx/sum(mx)
  return(mx)
}
my.pars <- rc_model_fund %>%
  select(value) %>% pull()

## derive the net female migrants by age (RC schedule)

## plotting crude growth rate



## END