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
# Update to include yours with ifelse

user <- Sys.info()[["user"]]

setwd(
  ifelse(
    user == "lizzie",
    "/Users/lizzie/Documents/git/projects/grephon/soilinoculation/analyses/",
    ifelse(
      user == "nolanmeier",
      "/Users/nolanmeier/git/morasoilinoculation/analyses"
    )
  )
)

# get the data!
# should be INPUT (not inputs)
aliveraw <- read.csv('inputs/alive_germinants_12_2.csv')
alive <- aliveraw
alive[is.na(alive)] <- 0 #  This works, all empty cells mean no germinants (of any kind) observed

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
# Hmm, this looks like a good start, but ...
	# it would be better if we summarized the data by inoluction level using ggplot stats etc.
	# would be great to work on that next if someone has time!
	# and then we could do it for dead also. 

####### Working off the above to sort by inoculation ##########
####### Dec. 29, 2025, by Nolan ########
head(alivelong)

# summarize stats by grouping by inoculation level
# plot as lines
ggplot(alivelong,
       aes(x = as.numeric(doy),
           y = germinants,
           color = inoculated,
           group = inoculated)) +
  
  stat_summary(fun = mean, geom = "line") + 
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 1
  ) + 
  facet_grid(seed_species ~ soil_species)
  
  


alive_10 <- alivelong[alivelong$inoculated == 10,]
alive_sterile <- alivelong[alivelong$inoculated == 0,]
alive_25 <- alivelong[alivelong$inoculated == 25,]

