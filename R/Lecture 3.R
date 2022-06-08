## -------------------------------------------- ##
## EDSD 2021-2022: Population projections
## Lecture 3
## Matrix Projections
## Date: 08/06/2022
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##


##----- MATRIX PROJECTION - FEMALES --------
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(viridis)

## set working directory in active folder (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading the data (derived from Preston et al. 2001)

load("data/EDSD.lecture2.Rdata")  

## extract sFx, NFx, bFx
m <- nrow(dta.swe)
sFx <- dta.swe$sFx
bFx <- dta.swe$bFx
NFx <- dta.swe$NFx

## remove last NA from sFx
sFx <- sFx[!is.na(sFx)]
length(sFx)  

## adjust bFx
bFx[m] <- 0

## create our Leslie matrix
L <- matrix(0,nrow=m,ncol=m)

## assign these two vectors to L
L[1,] <- bFx
diag(L[-1,]) <- sFx
L[m,m] <- sFx[length(sFx)]

## compute the projection
NFx5.matrix <- c(L%*%NFx)
NFx5.manual <- dta.swe$NFx5
all.equal(NFx5.matrix,NFx5.manual)

## create population matrix
pop.proj <- function(AgeGroup,bFx,sFx,NFx,n){
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
    N[,i+1] <- L%*%N[,i]
  }
  ## define our output
  out <- cbind(data.frame(AgeGroup=AgeGroup),N)
  return(out)
}

## checking that things work as expected

## define projection period
n <- 20
## actual projection
my.proj <- pop.proj(AgeGroup=dta.swe$AgeGroup,
                    bFx=bFx,sFx = sFx,
                    NFx = NFx,n=n)
all.equal(my.proj$"1",NFx)
all.equal(my.proj$"2",NFx5.manual)

## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(AgeGroup),names_to = "period",
               values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year))

## plotting with pyramid
library(scales)
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, Year %in% c(1993,1998,max(Year))),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(name = 'Year', 
                    values=c("lightblue","orange","darkgreen"))


## plotting with pyramid


##----- ANIMATION INTERMEZZO -------

## start by creating a multiple layer pdf

## saving plots in a single file


## using gganimate


## do with magick package
library(magick)

## create a directory to which the images will be written

## loop through years and write plot to file


## list file names and read in

## join the images together

## animate at 2 frames per second

## view animated image

## save to your pc

## SHINY APPS
library("shiny")

## general parameters

## build your user interfact

## build your server

## run the shiny app, which puts together the ui and server


## ADDING INTERACTIVITY
library("plotly")

## build your user interfact

## build your server

## run the shiny app, which puts together the ui and server


##----- MATRIX PROJECTION - MALES --------


## extract sMx, NMx, bMx

## adjust bMx

## remove last NA from sFx

## female Leslie matrix

## male Leslie matrix


## put together females and males

## making the projection


## let's do this with a function
## function to project several periods


## long data

## plotting


## saving the data for tomorrow's lecture
save.image("data/EDSD.lecture3.Rdata")

## END