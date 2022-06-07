## -------------------------------------------- ##
## EDSD 2021-2022: Population projections
## Lecture 1
## Introduction to population projections
## Date: 07/06/2022
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##

##----- EXERCISE 1 --------
rm(list=ls(all=TRUE))

## define the input of our exercise
N0_a <- 50
N0_b <- 35
r_a <- 0.05
r_b <- 0.15
t <- seq(0,10,0.01)

## function for our constant exponential growth model
PopProj <- function(N0,r,t){
  NT <- N0*(1+r)^t
  return(NT)
}

## computing the projected population at each time t
Na <- PopProj(N0=N0_a,r=r_a,t=t)
Nb <- PopProj(N0=N0_b,r=r_b,t=t)

## computing the time point when Nb is greater than Na
Nb>Na
which(Nb>Na)
that <- t[which(Nb>Na)[1]]

## plotting the results
par(mar=c(4,4,1,1))
plot(t,Na,t="l",ylim=range(Na,Nb),col=1,
     xlab="time (years)", ylab="population size")
lines(t,Nb,col=2)
abline(v=that,lty=2)
legend("topleft",c("pop A","pop B"),lty=1,
       col=1:2,cex=0.75)



##----- EXERCISE 2 --------
rm(list=ls(all=TRUE))

## define the input of our exercise
N0_a <- 50
N0_b <- 35
t <- seq(0,10,0.01)
b_a <- m_b <- 0.03
d_a <- 0.05
m_a <- 0.01
b_b <- 0.07
d_b <- 0.04

## function for our constant exponential growth model
PopProj <- function(N0,b,d,m,t){
  r <- b-d+m
  NT <- N0*(1+r)^t
  return(NT)
}


## computing the projected population at each time t
Na <- PopProj(N0=N0_a,b=b_a,d=d_a,m=m_a,t=t)
Nb <- PopProj(N0=N0_b,b=b_b,d=d_b,m=m_b,t=t)

## computing the time point when Nb is greater than Na
that <- t[which(Nb>Na)[1]]

## plotting the results
par(mar=c(4,4,1,1))
plot(t,Na,t="l",ylim=range(Na,Nb),col=1,
     xlab="time (years)", ylab="population size")
lines(t,Nb,col=2)
abline(v=that,lty=2)
legend("topleft",c("pop A","pop B"),lty=1,
       col=1:2,cex=0.75)



##----- EXERCISE 3 --------
rm(list=ls(all=TRUE))

## define the input of our exercise
N0 <- 50
t <- 1:21
y <- t-1
b <- 0.05
d <- 0.04
e <- 0.01
I <- 1.5


## function for our constant exponential growth model
PopProj <- function(N0,b,d,e,I,t){
  r <- b-d-e
  ## result vector
  NT <- rep(NA,length(t))
  ## assign the starting point
  NT[1] <- N0
  ## for loop
  for (i in 2:length(t)){
    NT[i] <- NT[i-1]*(1+r) + I
  }
  return(NT)
}

## computing the projected population at each time t
NT <- PopProj(N0,b,d,e,I,t)

## plotting the population
par(mar=c(4,4,1,1))
plot(t,NT,t="l",ylim=range(NT),col=1,
     xlab="time (years)", ylab="population size")

plot(y,NT,t="l",ylim=range(NT),col=1,
     xlab="time (years)", ylab="population size")

NT[length(NT)]
NT[max(t)]

## Incorporating future assumptions
e.assum <- c(rep(0.01,10),rep(0.02,11)) 
I.assum <- c(rep(1.5,10),seq(1.5,0,length.out=11)) 

## plotting our assumptions
plot(y,e.assum)
plot(y,I.assum)

## function for our constant exponential growth model
PopProj.v2 <- function(N0,b,d,e,I,t){
  ## result vector
  NT <- rep(NA,length(t))
  ## assign the starting point
  NT[1] <- N0
  ## for loop
  for (i in 2:length(t)){
    r <- b-d-e[i]
    NT[i] <- NT[i-1]*(1+r) + I[i]
  }
  return(NT)
}

## computing the projected population at each time t
## in this scenario
NT.v2 <- PopProj.v2(N0,b,d,e.assum,I.assum,t)

## plotting the population
par(mar=c(4,4,1,1))
plot(t,NT,t="l",ylim=range(NT),col=1,
     xlab="time (years)", ylab="population size")
lines(t,NT.v2,col=2,lty=2,lwd=2)
abline(v=10)

par(mar=c(4,4,1,1))
plot(y,NT,t="l",ylim=range(NT),col=1,
     xlab="time (years)", ylab="population size")
lines(y,NT.v2,col=2,lty=2,lwd=2)
abline(v=10)

## plotting only the last ten years



## END