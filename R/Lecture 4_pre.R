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


## assume a total of 25000 net migration counts
I <- 2.5e4
mx <- mxRG(x=dta.swe$Age,pars=my.pars)
plot(dta.swe$Age,mx)
Ix <- I*mx
sum(Ix)
plot(dta.swe$Age, Ix, type="o",pch=16,
     xlab = "Age group",ylab= "Net migrant counts",
     main="RC migration schedule for 25,000 net migrants")


## create population matrix function with MIGRATION
pop.proj.MIG <- function(AgeGroup,bFx,sFx,NFx,Ix,n){
  ## dimension of age group vector
  m <- length(AgeGroup)
  ## create our Leslie matrix
  L <- matrix(0,nrow=m,ncol=m)
  ## assign these two vectors to L
  L[1,] <- bFx
  diag(L[-1,]) <- sFx
  L[m,m] <- sFx[length(sFx)]
  ## create matrix of population counts
  N <- matrix(NA,nrow = m,ncol = (n+1))
  ## assign starting population to first column of N
  N[,1] <- NFx
  ## for loop to project population n times
  for (i in 1:n){
    N[,i+1] <- L%*%(N[,i]+Ix/2) + Ix/2
  }
  ## define our output
  out <- cbind(data.frame(AgeGroup=AgeGroup),N)
  return(out)
}

## define projection period
n <- 20
## actual projection
my.proj <- pop.proj(AgeGroup=dta.swe$AgeGroup,
                    bFx=bFx,sFx = sFx,
                    NFx = NFx,n=n)
my.proj.mig <- pop.proj.MIG(AgeGroup=dta.swe$AgeGroup,
                            bFx=bFx,sFx = sFx,
                            NFx = NFx,Ix=Ix,n=n)

## long data format
dta.swe.l <- my.proj %>%
  pivot_longer(-c(AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="baseline")
dta.swe.l.mig <- my.proj.mig %>%
  pivot_longer(-c(AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="with migration")

## combine the long data
dta.swe.all <- dta.swe.l %>% 
  bind_rows(dta.swe.l.mig)

## plotting with pyramid
ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
  geom_bar(data = subset(dta.swe.all, Year == max(Year)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population 2093") +
  scale_fill_manual(name = 'Assumption', values=c("lightblue","orange"))


##----- EXERCISE 3: TIME-VARYING assumptions on TFR --------

## extract the Fx
x <- dta.swe$Age
fx <- dta.swe$Fx
tfr.curr <- 5*sum(fx)
FX <- matrix(fx,nrow = m,ncol=n)
tfr <- seq(tfr.curr,0.75,length.out=20)
plot(1:20,tfr)

i <- 2
for (i in 1:n){
  FX[,i] <- (FX[,i]/tfr.curr) * tfr[i]
}

matplot(x,FX,t="l",lty=1,col=viridis(n))
5*apply(FX,2,sum)




## END