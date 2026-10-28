# Started Oct 27th 
# Started by Jaslyne Marek
# Preliminary analyses and plotting for MORA soil inoculation experiment
# Using germ_analyses as a reference


# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

# libraries
library(ggplot2)
library(dplyr)
library(reshape)

# setting working directory

user <- Sys.info()[["user"]]
setwd("/Users/jasly/Git/morasoilinoculation/analyses")

# loading in the data!

alive <- read.csv('inputs/alive_germinants_10_27.csv')
alive[is.na(alive)] <- 0

dead <- read.csv('inputs/dead_germinants_10_27.csv')
dead[is.na(dead)] <- 0 # all empty cells mean no germinants (of any kind) observed

#filtering out columns with halfdead data
dead_nohalf <- dead[, !grepl("halfdead", names(dead))]

# renaming DOY colnames to just be the numeric value
col_names_alive <- gsub("^X(\\d+)_alive$", "\\1", colnames(alive))
colnames(alive) <- (col_names_alive)
head(alive)

col_names_dead <- gsub("^X(\\d+)_dead$", "\\1", colnames(dead_nohalf))
colnames(dead)<- (col_names_dead)


# cleaning up DOY to day since beginning of experiment, assuming seeds were planted on Sep 10th (DOY 253) according to soilinoculationDailyLog.txt


