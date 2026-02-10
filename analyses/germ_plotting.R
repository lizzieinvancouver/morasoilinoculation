## Started Boxing Day 2025 ##
## By Lizzie ##

## Working off germ_analysis a little ... but plotting##

# housekeeping
rm(list=ls())
options(stringsAsFactors=FALSE)

# Load libraries
library(ggplot2)
library(reshape)
library(tidyverse)

# set wd
# Update to include yours with ifelse

user <- Sys.info()[["user"]]

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
)

# get the data!
# should be INPUT (not inputs)

# First, bring in full, most recent dataset (will talk to Mao about lab protocols for doing this)
seedsraw <- read.csv('inputs/germination_data.csv') # Full data set with alive and dead, from Jan. 2
seeds <- seedsraw[1:210,]
seeds[is.na(seeds)] <- 0 #  This works, all empty cells mean no germinants (of any kind) observed


seeds$seed_species <- str_trim(seeds$seed_species, side = "right")

# filter out all columns containing dead data
aliveraw <- seeds[, !grepl("dead", names(seeds))]
alive <- aliveraw

# filter out all columns containing alive data
deadraw <- seeds[, !grepl("alive", names(seeds))]
dead <- deadraw


# reshaping
library(reshape)
alivelong <-  melt(alive, id.var=c("soil_species", "seed_species", "inoculated_.", "pot", "notes"))
names(alivelong) <- c("soil_species", "seed_species", "inoculated", "pot", "notes", "doy", "germinants")

deadlong <- melt(dead, id.var=c("soil_species", "seed_species", "inoculated_.", "pot", "notes"))
names(deadlong) <- c("soil_species", "seed_species", "inoculated", "pot", "notes", "doy", "germinants")

# clean up the variable 
alivelong$doy <- substr(alivelong$doy, 2, 4)
deadlong$doy <- substr(deadlong$doy, 2, 4)
unique(alivelong$doy)

#quartz()
ggplot(alivelong, aes(x=as.numeric(doy), y=germinants, color=as.factor(inoculated))) +
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
aliveplot <- ggplot(alivelong,
       aes(x = as.numeric(doy),
           y = germinants,
           color = as.factor(inoculated),
           group = inoculated)) +
  
  stat_summary(fun = mean, geom = "line") + 
  
  # to remove error bars
  # stat_summary(
  #   fun.data = mean_se,
  #   geom = "errorbar",
  #   width = 1
  # ) + 
  
  facet_grid(seed_species ~ soil_species)
  

##### Adding to the above but trying to start with the full data set ######
###### January 2, 2025, by Nolan ######

deadplot <- ggplot(deadlong,
       aes(x = as.numeric(doy),
           y = germinants,
           color = as.factor(inoculated),
           group = inoculated)) +
  
  stat_summary(fun = mean, geom = "line") + 
  
  # to remove error bars
  # stat_summary(
  #   fun.data = mean_se,
  #   geom = "errorbar",
  #   width = 1
  # ) + 
  
  facet_grid(seed_species ~ soil_species) 

####### Jan. 11, 2026 #########
###### Nolan, working to modify graphs #######

# Set the colour scale to match the tray labels 
deadplot + scale_color_manual(values=c("green", "salmon", "blue"))
aliveplot + scale_color_manual(values=c("green", "salmon", "blue"))

##### January 16, 2026 #########
# Combining all soil/seed treatments to only show inoculation effects, Nolan
germinants_by_inoculation <- alivelong %>%
  group_by(soil_species,seed_species) %>%
  ggplot(aes(x = as.numeric(doy),
      y = germinants,
      color = as.factor(inoculated),
      group = inoculated)) +
  
  stat_summary(fun = mean, geom = "line") + 
  scale_color_manual(values=c("green", "salmon", "blue"))
germinants_by_inoculation  

deaths_by_inoculation <- deadlong %>%
  group_by(soil_species,seed_species) %>%
  ggplot(aes(x = as.numeric(doy),
             y = germinants,
             color = as.factor(inoculated),
             group = inoculated)) +
  
  stat_summary(fun = mean, geom = "line") + 
  scale_color_manual(values=c("green", "salmon", "blue"))
deaths_by_inoculation


##### Working to do some more visualisations/calculations ######
## Jan. 31, Nolan #######

##### Add a column for plot_id to group by treatment
alivelong <- alivelong %>%
  mutate(germ_prop = germinants/10) %>%
  mutate(pot_id = paste(soil_species, seed_species,inoculated,sep = "_"))


T50 <- alivelong %>%
  arrange(pot_id,doy)%>%
  group_by(pot_id,pot)%>%
  filter(germ_prop >= 0.5)%>%
  slice(1)%>%
  ungroup()%>%
  select(pot_id,pot,t50_day = doy)
  


  #facet_wrap()
### Add the other variables to the df so they can be used for facet_wrap #####
T50_treatments <- T50%>%
  #group_by(pot_id)%>%
  #summarise(mean_t50 = mean(as.numeric(t50_day)))%>%
  mutate(soil_species = substr(pot_id,1,4))%>%
  mutate(seed_species = substr(pot_id,6,9))%>%
  mutate(inoculated = as.numeric(substr(pot_id,11,12)))%>%
  arrange(seed_species,soil_species)

T50_stats <- T50_treatments %>%
  group_by(pot_id)%>%
  summarise(mean_t50 = mean(as.numeric(t50_day)),
            sd_t50 = sd(as.numeric(t50_day)))%>%
  mutate(soil_species = substr(pot_id,1,4))%>%
  mutate(seed_species = substr(pot_id,6,9))%>%
  mutate(inoculated = as.numeric(substr(pot_id,11,12)))

ggplot(T50_stats,aes(x=inoculated,y=mean_t50,group=pot_id))+
  geom_point()+
  geom_errorbar(aes(ymin = mean_t50 - sd_t50,
                    ymax = mean_t50 + sd_t50)) + 
  facet_wrap(seed_species~soil_species)



######## Nolan, Feb. 9, 2026 #######
### Adding splines to the scatter plots done previously by Lizzie

spline_0 <- alivelong %>%
  filter(inoculated == 0)%>%
  ggplot(aes(x=as.numeric(doy), y=germinants, color=as.factor(inoculated))) +
  geom_point() + 
  geom_smooth(method = "gam",formula = y~s(x),se=FALSE)+
  facet_grid(seed_species~soil_species) + 
  scale_color_manual(values=c("darkgreen", "salmon", "skyblue"))
spline_0

spline_10 <- alivelong %>%
  filter(inoculated == 10)%>%
  ggplot(aes(x=as.numeric(doy), y=germinants, color=as.factor(inoculated))) +
  geom_point() + 
  geom_smooth(method = "gam",formula = y~s(x),se=FALSE)+
  facet_grid(seed_species~soil_species) + 
  scale_color_manual(values=c("salmon"))
spline_10

spline_25 <- alivelong %>%
  filter(inoculated == 25)%>%
  ggplot(aes(x=as.numeric(doy), y=germinants, color=as.factor(inoculated))) +
  geom_point() + 
  geom_smooth(method = "gam",formula = y~s(x),se=FALSE)+
  facet_grid(seed_species~soil_species) + 
  scale_color_manual(values=c("blue"))
spline_25


deadlong %>%
  filter(doy==393)%>%
  group_by(seed_species)%>%
  summarise(mean_dead = mean(germinants))

deadlong %>%
  filter(doy==393)%>%
  group_by(soil_species)%>%
  summarise(mean_dead = mean(germinants))

deadlong %>%
  filter(doy==393)%>%
  group_by(inoculated)%>%
  summarise(mean_dead = mean(germinants))
