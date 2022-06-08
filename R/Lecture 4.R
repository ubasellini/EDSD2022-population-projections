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

i <- 1
test <- FX[-m,i]+sFx*FX[-1,i]
length(sFx)
length(FX[,i])
FX[-1,i]
## create function for TFR assumption
## create population matrix
pop.proj.TFR <- function(AgeGroup,srb,L0F,l0,FX,sFx,NFx,n){
  ## dimension of age group vector
  m <- length(AgeGroup)
  ## create our Leslie matrix
  L <- matrix(0,nrow=m,ncol=m)
  ## assign these two vectors to L
  diag(L[-1,]) <- sFx
  L[m,m] <- sFx[length(sFx)]
  ## create elemnts to compute bFx
  srb.fact <- 1/(1+srb)
  L0.fact <- L0F/(2*l0)
  ## create matrix of population counts
  N <- matrix(NA,nrow = m,ncol = (n+1))
  ## assign starting population to first column of N
  N[,1] <- NFx
  ## for loop to project population n times
  i <- 1
  for (i in 1:n){
    bFx <- srb.fact*L0.fact*(FX[-m,i]+sFx*FX[-1,i])
    L[1,] <- c(bFx,0)
    N[,i+1] <- L%*%N[,i]
  }
  ## define our output
  out <- cbind(data.frame(AgeGroup=AgeGroup),N)
  return(out)
}


## projection
my.proj.TFR <- pop.proj.TFR(AgeGroup=dta.swe$AgeGroup,
                            srb=srb,l0=l0,
                            NFx=NFx,sFx=sFx,FX=FX,
                            L0F=L0F,n=n)



## long data format
dta.swe.l <- my.proj %>%
  pivot_longer(-c(AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="baseline")
dta.swe.l.tfr <- my.proj.TFR %>%
  pivot_longer(-c(AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="TFR assumption")

## combine the long data
dta.swe.all <- dta.swe.l %>% 
  bind_rows(dta.swe.l.tfr)

## plotting with pyramid
ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
  geom_bar(data = subset(dta.swe.all, period == 21),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population 2093") +
  scale_fill_manual(name = 'Assumption', values=c("#E69F00", "#56B4E9"))


## SHINY APP for dynamic visualization of your results
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(dta.swe.l$Year), min = min(dta.swe.l$Year), max = max(dta.swe.l$Year)),
  column(12, plotOutput("plot_pyr1"))
)
server <- function(input, output){
  output$plot_pyr1 <- renderPlot({
    ## plotting pyramid
    ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
      geom_bar(data = subset(dta.swe.all, Year == input$year),
               stat = "identity",position = "dodge",color = "black") +
      coord_flip() +
      theme_bw() +
      ggtitle(paste("Swedish female population, year",subset(dta.swe.all, Year == input$year)$Year)) +
      scale_fill_manual(name = 'Projection', values=c("#E69F00", "#56B4E9","#1C7C54")) +
      scale_y_continuous(limits = c(0, 350000), breaks = seq(0, 350000, 100000))
  })
}
shinyApp(ui = ui, server = server)


##----- EXERCISE 4: stable population and Leslie matrix --------

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

## longer time horizon for projections
n <- 50
my.proj <- pop.proj.v2(AgeGroup=dta.swe$AgeGroup,
                       NFx=NFx,sFx=sFx,bFx=bFx,
                       NMx=NMx,sMx=sMx,bMx=bMx,n=n)
## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(AgeGroup,sex),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))

## compute total population by sex in each year
tot.pop <- dta.swe.l %>%
  group_by(YearF) %>%
  summarise(TotPop=sum(population))

## compute distribution of the population in each age group
dta.swe.l <- dta.swe.l %>%
  left_join(tot.pop) %>%
  mutate(Pch=population/TotPop)

## extract distribution in last year
x <- dta.swe$Age
xG <- dta.swe$AgeGroup
yF <- dta.swe.l$Pch[dta.swe.l$Year == max(dta.swe.l$Year) & dta.swe.l$sex == "Females"]
yM <- dta.swe.l$Pch[dta.swe.l$Year == max(dta.swe.l$Year) & dta.swe.l$sex == "Males"]

plot(x,yF)

## eigendecomposition of the Leslie matrix
ev.decomp <- eigen(L)

## extract largest eigenvalue
## this will give you the long-term CGR of the stable population
ev.val <- abs(ev.decomp$values)[1]
cgr_leslie <- log(ev.val)

## extract corresponding eigenvectors
ev.vec <- ev.decomp$vectors[,1]
ev.vecF <- ev.vec[1:m]
ev.vecM <- ev.vec[1:m + m]

## rescale them to sum to the long-term sex-specific total distribution
## this will give you the long-term distribution of the stable population
ev.vecF <- (ev.vecF/sum(ev.vecF)) * sum(yF)
ev.vecM <- (ev.vecM/sum(ev.vecM)) * sum(yM)

## compare Male distribution of projection vs Leslie
plot(x,yM,t="n",axes = F,xlab = "age group",ylab = "",
     ylim = range(yM,yF),main="Males",cex.main=1.5)
axis(1)
axis(2);grid();box()
mtext("Pop distribution",side=2,cex=1.5,line=2.4,las=3)
points(x,yM,lwd=2)
points(x,ev.vecM,col=4,pch=4,lwd=2)
legend("bottomleft",c("From projections","From Leslie"),pch=c(1,4),col=c(1,4),
       lwd=2,cex=1.25,inset = 0.01,lty=NA,bg="white")



## extract overall total population
tot.pop <- tot.pop %>%
  pull(TotPop)

## compute growth rate
CGR <- rep(NA,n)
for (i in 1:n){
  CGR[i] <- log(tot.pop[i+1]/tot.pop[i])
}

## plotting crude growth rate
plot(1:n,CGR,col=4,t="l",lwd=2)
abline(h=cgr_leslie,col=2,lty=2,lwd=2)
legend("topright",c("From projections","From Leslie"),pch=NA,col=c(4,2),
       lwd=2,cex=1.25,lty=c(1,2),bg="white")



## END