# Started November 6th, 2026
# Started by Jaslyne
# Taking raw .xslx data and cleaning it to add total germination %, t50 where 50% of seeds have germinated, and t50 where 50% of germinants have died.

# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

# libraries
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)

# set wd
user <- Sys.info()[["user"]]
setwd("/Users/jasly/Git/morasoilinoculation")


# loading in data

germ_raw <- read_csv("analyses/inputs/germination_data_messy.csv")

# getting rid of two rows with no data

germ_raw <- germ_raw[-c(211,212), ]

# making it so that n/a comes in as a 0

# first changing all germinant/death data to numerical from logical

lgl_cols <- sapply(germ_raw, is.logical)

germ_raw[lgl_cols] <- lapply(germ_raw[lgl_cols], as.numeric)

# now selecting all of the numeric columns, and applying is.na() <- 0

num_cols <- sapply(germ_raw, is.numeric)

germ_raw[num_cols] <- lapply(germ_raw[num_cols], function(x) {x[is.na(x)] <- 0; x})

germ_raw$total_germ <- germ_raw$"238_alive" + germ_raw$"238_dead" + germ_raw$"238_halfdead5"

germ_raw$'total_germ_%' <- germ_raw$total_germ / 10 * 100

# changing the total germination percentages for pots with more than 10 seeds to 100, assume that all seeds germinated in this case.

germ_raw[18, "total_germ_%"] <- 100
germ_raw[24, "total_germ_%"] <- 100
germ_raw[46, "total_germ_%"] <- 100
germ_raw[141, "total_germ_%"] <- 100
germ_raw[166, "total_germ_%"] <- 100



