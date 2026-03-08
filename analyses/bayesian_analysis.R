#### Trying weibull bayesian model based on Dan's Code ####
rm(list=ls())
options(stringsAsFactors=FALSE)

### Pull base libraries ####
library(ggplot2)
library(reshape)
library(tidyverse)


### Pull libraries for bayesian ####
library(drc)
library(lme4)
library(brms)
library(ggstance)
library(tidybayes)
library(bayesplot)


## Set wd
user <- Sys.info()[["user"]]
setwd(
ifelse(
  user == "lizzie",
  "/Users/lizzie/Documents/git/projects/grephon/soilinoculation/analyses/",
  ifelse(
    user == "nolanmeier",
    "/Users/nolanmeier/git/morasoilinoculation/analyses",
    ifelse(
      user == "Xiaomao", 
      "C:/PhD/Project/morasoilinoculation/analyses",
      NA
    )
  )
))

getwd() ### Sanity check 


### Pull in most recent data
seedsraw <- read.csv('inputs/germination_data.csv') # Full data set with alive and dead, from Jan. 2
seeds <- seedsraw[1:210,]
seeds[is.na(seeds)] <- 0 


seeds$seed_species <- str_trim(seeds$seed_species, side = "right")

# filter out all columns containing dead data
aliveraw <- seeds[, !grepl("dead", names(seeds))]
alive <- aliveraw

# filter out all columns containing alive data
deadraw <- seeds[, !grepl("alive", names(seeds))]
dead <- deadraw

priorz.wei<-get_prior(DAY | cens(censored)~chillweeks+force+(chillweeks+force|Taxa),data=d,family= weibull())
fit.wei.all<- brm(DAY | cens(censored)~chillweeks+force+(chillweeks+force|Taxa), data=d, family =   weibull(),inits=0 ,prior=priorz.wei,iter=4000,warmup = 3000, control=list(adapt_delta=0.95),chains=4) 
fixef(fit.wei.all)
colnames(d)
forknb<-dplyr::select(d,Taxa, plate_num, DAY, germinated, chillweeks,force,censored)
forknb<-dplyr::filter(forknb, !Taxa %in% c("Cryptotaenia candensis","Hesperis matronalis"))



