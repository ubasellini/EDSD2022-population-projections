## -------------------------------------------- ##
## EDSD 2021-2022: Population projections
## Lecture 2
## The cohort component method
## Date: 07/06/2022
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##


##----- EXERCISE 1 --------
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)

## set working directory in active folder (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading the data (derived from Preston et al. 2001)
dir()
load("data/dta.swe.1993.Rdata")  

## explore the data
head(dta.swe)

## project the age groups forward
dta.swe <- as_tibble(dta.swe) %>% 
  mutate(sFx=lead(LFx)/LFx,
         NFx5=lag(sFx*NFx))

dta.swe$sFx

## adjusting the last age group
dta.swe <- dta.swe %>% 
  mutate(sFx=ifelse(test = Age==80,
                    yes  = lead(LFx)/(LFx+lead(LFx)),
                    no   = sFx),
         NFx5=ifelse(test = Age==85,
                     yes  = (NFx+lag(NFx))*lag(sFx),
                     no   = NFx5))

## checking the results
head(dta.swe[,c(1:4,8,9)])
tail(dta.swe[,c(1:4,8,9)])

## some inputs for adjusting the first age group
srb <- 1.05
fact.srb <- 1/(1+srb)
l0 <- 1e5
L0F <- dta.swe$LFx[dta.swe$Age==0]
L0F.vec <- dta.swe %>% 
  select(LFx) %>% pull() 
L0F <- L0F.vec[1]

## adjust the first age group
dta.swe <- dta.swe %>% 
  mutate(bFx=fact.srb*L0F/(2*l0)*(Fx+sFx*lead(Fx)),
         Bx=Fx*5*(NFx+NFx5)/2,
         NFx5=ifelse(test = Age==0,
                     yes  = fact.srb*L0F/(5*l0)*sum(Bx,na.rm=T),
                     no   = NFx5))

dta.swe$NFx5[1]
sum(dta.swe$NFx*dta.swe$bFx,na.rm=T)

## first example of pyramid plotting

## long data format
dta.swe.l <- dta.swe %>% 
  select(AgeGroup,NFx,NFx5) %>% 
  rename('1993'=NFx,'1998'=NFx5) %>% 
  pivot_longer(-AgeGroup,names_to = "year",
               values_to = "population")

## plot a pyramid
library(scales)
dta.swe.l %>% 
  ggplot(aes(x=AgeGroup,y=population,fill=year))+
  geom_bar(stat="identity",position="dodge",color="black")+
  coord_flip()+
  theme_bw()+
  ggtitle("Swedish female population")+
  scale_fill_manual(values=c("lightblue","orange"))+
  scale_y_continuous(labels = comma)


## CASE of MALE population

## some inputs for adjusting the first age group

## projecting the male population

## checking the results


## saving the data for tomorrow's lecture
save.image("data/EDSD.lecture2.Rdata")

## END