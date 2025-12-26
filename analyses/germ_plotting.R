## Started Boxing Day 2025 ##
## By Lizzie ##

## Working off germ_analysis a little ... but plotting##

# housekeeping
rm(list=ls())
options(stringsAsFactors=FALSE)

# Load libraries
library(ggplot2)
library(reshape)


# set wd
setwd("/Users/lizzie/Documents/git/projects/grephon/soilinoculation/analyses/")

# get the data!
# should be INPUT (not inputs)
aliveraw <- read.csv('inputs/alive_germinants_12_2.csv')
alive <- aliveraw
alive[is.na(alive)] <- 0 #  NA -> 0, is this okay?

dead <- read.csv('inputs/dead_germinants_12_2.csv')

# reshaping
library(reshape)
alivelong <-  melt(alive, id.var=c("soil_species", "seed_species", "inoculated_.", "pot", "notes"))
names(alivelong) <- c("soil_species", "seed_species", "inoculated", "pot", "notes", "doy", "germinants")

# clean up the variable 
alivelong$doy <- substr(alivelong$doy, 2, 4)
unique(alivelong$doy)

quartz()
ggplot(alivelong, aes(x=as.numeric(doy), y=germinants, color=inoculated)) +
	geom_point() + 
	facet_grid(seed_species~soil_species)
# Hmm, this looks like a good start, but it would be better if we summarized the data by inoluction level using ggplot stats etc.