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
alive <- subset(alive, seed_species != 0)
alive <- subset(alive, soil_species != 0)

dead <- read.csv('inputs/dead_germinants_10_27.csv')
dead[is.na(dead)] <- 0 # all empty cells mean no germinants (of any kind) observed

#filtering out columns with halfdead data
dead_nohalf <- dead[, !grepl("halfdead", names(dead))]

dead_nohalf <- subset(dead_nohalf, soil_species != 0)
dead_nohalf <- subset(dead_nohalf, seed_species != 0)

# renaming DOY colnames to just be the numeric value
col_names_alive <- gsub("^X(\\d+)_alive$", "\\1", colnames(alive))
colnames(alive) <- (col_names_alive)


col_names_dead <- gsub("^X(\\d+)_dead$", "\\1", colnames(dead_nohalf))
colnames(dead_nohalf)<- (col_names_dead)


# cleaning up DOY to day since beginning of experiment, assuming seeds were planted on Sep 10th (DOY 253) according to soilinoculationDailyLog.txt

# selecting columns with DOY
target_cols <- c(7:49)

# making a vector with days since start of experiment (from sep 10th, 2025)
day_from_start <- c(43,	47,	50,	54,	57,	61,	64,	68,	71,	75,	78,	82,	85,	89,	93,	98,	105, 112,	119, 126, 133, 140,	147, 154, 161, 168,	175, 182,	189, 196,	204, 211,	217, 224,	238, 252, 266, 280,	294, 308,	322, 336, 350)

# switching the column names from DOY to day since experiment
colnames(alive)[target_cols] <- day_from_start

colnames(dead_nohalf)[target_cols] <- day_from_start



# reshaping
alivelong <- melt(alive, id.var=c("soil_species", "seed_species", "inoculated_.", "pot", "notes"), measure.vars = c("43",	"47",	"50",	"54",	"57",	"61",	"64",	"68",	"71",	"75",	"78",	"82",	"85",	"89",	"93",	"98",	"105", "112",	"119", "126", "133", "140",	"147", "154", "161", "168",	"175", "182",	"189", "196",	"204", "211",	"217", "224",	"238", "252", "266", "280",	"294", "308",	"322", "336", "350"), variable.name = "day_from_start", value.name = "germinants")
names(alivelong) <- c("soil_species", "seed_species", "inoculated", "pot", "notes","day_from_start", "germinants")


deadlong <- melt(dead_nohalf, id.var=c("soil_species", "seed_species", "inoculated_.", "pot", "notes"), measure.vars = c("43",	"47",	"50",	"54",	"57",	"61",	"64",	"68",	"71",	"75",	"78",	"82",	"85",	"89",	"93",	"98",	"105", "112",	"119", "126", "133", "140",	"147", "154", "161", "168",	"175", "182",	"189", "196",	"204", "211",	"217", "224",	"238", "252", "266", "280",	"294", "308",	"322", "336", "350"), variable.name = "day_from_start", value.name = "germinants")
names(deadlong) <- c("soil_species", "seed_species", "inoculated", "pot", "notes", "day_from_start", "germinants")

alivelong$day_from_start <- as.numeric(as.character(alivelong$day_from_start))

deadlong$day_from_start <- as.numeric(as.character(deadlong$day_from_start))

# plotting germination by treatment
germination_by_treatment <- ggplot(alivelong, aes(x=as.numeric(day_from_start), y=germinants, color=as.factor(inoculated)))+
  geom_point() +
  facet_grid(seed_species~soil_species) +
  coord_cartesian(xlim = c(0,350))


ggsave(file = "./figures/updated_germination_by_treatment.pdf", plot = germination_by_treatment, dpi = 800, units = "mm", width = 150, height = 100)


# plotting deaths by treatment
deaths_by_treatment <- ggplot(deadlong, aes(x=as.numeric(day_from_start), y=germinants, color=as.factor(inoculated)))+
  geom_point() +
  facet_grid(seed_species~soil_species) +
  coord_cartesian(xlim = c(0,350))

ggsave(file = "./figures/updated_deaths_by_treatment.pdf", plot = deaths_by_treatment, dpi = 800, units = "mm", width = 150, height = 100)
