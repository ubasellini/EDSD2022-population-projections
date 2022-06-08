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



##----- ANIMATION INTERMEZZO -------

## start by creating a multiple layer pdf
plots <- list()
my.cols <- cividis(n+1)
my.years <- unique(dta.swe.l$Year)
i <- 1
for (i in 1:(n+1)){
  gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
    geom_bar(data = subset(dta.swe.l, period == i),
             stat = "identity",color = "black") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(paste("Swedish female population, year",my.years[i])) +
    scale_fill_manual(values=my.cols[i])
  plots[[i]] <- gg
}
## saving plots in a single file
pdf("figs/myAnimFig.pdf")
invisible(lapply(plots, print))
dev.off()

## using gganimate
## gganimate
library(gganimate)
library(gifski)
gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,
                           fill=YearF)) +
  geom_bar(stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option="cividis")

gg + transition_states(YearF) +
  ggtitle('Swedish female population, year {closest_state}')

anim_save("figs/F2.gif")

## do with magick package
library(magick)

## create a directory to which the images will be written
dir_out <- file.path(tempdir(), "temp_dir")
dir.create(dir_out, recursive = TRUE)

## loop through years and write plot to file
for (i in 1:(n+1)){
  gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
    geom_bar(data = subset(dta.swe.l, period == i),
             stat = "identity",color = "black") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(paste("Swedish female population, year",my.years[i])) +
    scale_fill_manual(values=my.cols[i])
  fp <- file.path(dir_out, paste0(i, ".png"))
  ggsave(plot = gg, 
         filename = fp, 
         device = "png")
}


## list file names and read in
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)
## view animated image
img_animated
## save to your pc
image_write(image = img_animated,
            path = "figs/example.gif")

## SHINY APPS ---> see 
library("shiny")



##----- MATRIX PROJECTION - MALES --------

## extract sMx, NMx, bMx
sMx <- dta.swe$sMx
bMx <- dta.swe$bMx
NMx <- dta.swe$NMx

## adjust bMx
bMx[is.na(bMx)] <- 0

## remove last NA from sFx
sMx <- sMx[!is.na(sMx)]

## female Leslie matrix
LF <- L

## male Leslie matrix
BM <- LM <- matrix(0,m,m)
BM[1,] <- bMx

diag(LM[-1,]) <- sMx
LM[m,m] <- sMx[m-1]

## put together females and males
ZEROS <- diag(0,m)
Lup <- cbind(LF,ZEROS)
Ldown <- cbind(BM,LM)
L <- rbind(Lup,Ldown)

## making the projection
Nx <- c(NFx,NMx)
Nx5.matrix <- c(L%*%Nx)

NFx5.manual <- dta.swe$NFx5
NMx5.manual <- dta.swe$NMx5
all.equal(NFx5.manual,Nx5.matrix[1:m]) 
all.equal(NMx5.manual,Nx5.matrix[1:m+m]) 

## let's do this with a function
## function to project several periods
pop.proj.v2 <- function(AgeGroup,NFx,sFx,bFx,NMx,sMx,bMx,n){
  ## number of age groups
  m <- length(AgeGroup)
  m2 <- m*2
  ## female Leslie matrix
  LF <- matrix(0,m,m)
  LF[1,] <- bFx
  diag(LF[-1,]) <- sFx
  LF[m,m] <- sFx[m-1]
  ## male Leslie matrix
  BM <- LM <- matrix(0,m,m)
  BM[1,] <- bMx
  diag(LM[-1,]) <- sMx
  LM[m,m] <- sMx[m-1]
  ## putting them together
  ZEROS <- diag(0,m)
  Lup <- cbind(LF,ZEROS)
  Ldown <- cbind(BM,LM)
  L <- rbind(Lup,Ldown)
  ## create population matrix
  N <- matrix(0,m2,n+1)
  N[,1] <- c(NFx,NMx)
  for (i in 1:n){
    N[,i+1] <- L%*%N[,i]
  }
  out <- cbind(data.frame(AgeGroup=rep(AgeGroup,2),
                          sex=rep(c("Females","Males"),each=m)),N)
  return(out)
}


## project
my.proj <- pop.proj.v2(AgeGroup=dta.swe$AgeGroup,NFx=NFx,sFx=sFx,bFx=bFx,
                       NMx=NMx,sMx=sMx,bMx=bMx,n=20)
## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(AgeGroup,sex),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))
## plotting
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21) & sex == "Males"),
           stat = "identity",position = "dodge",color = "black",mapping = aes(y = -population)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21) & sex == "Females"),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish population") +
  scale_y_continuous(limits=c(-3.5e5,3.5e5),
                     breaks = seq(-4e5,4e5,1e5),
                     labels = abs(seq(-4e5,4e5,1e5))) +
  scale_fill_brewer(name="Year",palette = 'Blues', direction = -1) +
  geom_text(data = subset(dta.swe.l, period %in% c(1)),
            aes(y = max(population)/1.25, x = 17, label='Females'),size=7) +
  geom_text(data = subset(dta.swe.l, period %in% c(1)),
            aes(y = -max(population)/1.25, x = 17, label='Males'),size=7)


## saving the data for tomorrow's lecture
save.image("data/EDSD.lecture3.Rdata")

## END