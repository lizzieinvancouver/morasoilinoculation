library(ggplot2)
library(dplyr)

setwd("/Users/nolanmeier/git/morasoilinoculation/Analyses/inputs")

alive <- read.csv('alive_germinants_12_2.csv')
alive <- alive %>% replace(is.na(.), 0)

dead <- read.csv('dead_germinants_12_2.csv')
dead <- dead %>% replace(is.na(.),0)
head(alive)

# To modify as expermient gains more days
days_of_experiment <- c(296,300,303,307,310,314,317,321,324,328,331,335)

############ Number of seeds per treatment group, for ref. ###############
# 2100 seeds in total in experiment
# 1050 seeds of each species (0.5 multiplier for seeds)
# 900 seeds in each of 0 and 10% inoculum treatments (1800 total)
# 300 seeds in 25% inoculum treatments
# 700 seeds in each soil species treatment group (1/3 multiplier)


#Separates data into TSHE and PSME seed treatments
TSHE_alive <- filter(alive, seed_species == "TSHE")[,6:17]
PSME_alive <- filter(alive, seed_species == "PSME")[,6:17]

# Identifies all individual treatment combinations, removes non-numerical columns
TSHE_TSHE_0 <- filter(alive, soil_species == "TSHE",seed_species == "TSHE", inoculated_. == 0)[,6:17]
THPL_TSHE_0 <- filter(alive, soil_species == "THPL",seed_species == "TSHE", inoculated_. == 0)[,6:17]
PSME_TSHE_0 <- filter(alive, soil_species == "PSME",seed_species == "TSHE", inoculated_. == 0)[,6:17]
TSHE_PSME_0 <- filter(alive, soil_species == "TSHE",seed_species == "PSME", inoculated_. == 0)[,6:17]
THPL_PSME_0 <- filter(alive, soil_species == "THPL",seed_species == "PSME", inoculated_. == 0)[,6:17]
PSME_PSME_0 <- filter(alive, soil_species == "PSME",seed_species == "PSME", inoculated_. == 0)[,6:17]

TSHE_TSHE_10 <- filter(alive, soil_species == "TSHE",seed_species == "TSHE", inoculated_. == 10)[,6:17]
THPL_TSHE_10 <- filter(alive, soil_species == "THPL",seed_species == "TSHE", inoculated_. == 10)[,6:17]
PSME_TSHE_10 <- filter(alive, soil_species == "PSME",seed_species == "TSHE", inoculated_. == 10)[,6:17]
TSHE_PSME_10 <- filter(alive, soil_species == "TSHE",seed_species == "PSME", inoculated_. == 10)[,6:17]
THPL_PSME_10 <- filter(alive, soil_species == "THPL",seed_species == "PSME", inoculated_. == 10)[,6:17]
PSME_PSME_10 <- filter(alive, soil_species == "PSME",seed_species == "PSME", inoculated_. == 10)[,6:17]

TSHE_TSHE_25 <- filter(alive, soil_species == "TSHE",seed_species == "TSHE", inoculated_. == 25)[,6:17] 
THPL_TSHE_25 <- filter(alive, soil_species == "THPL",seed_species == "TSHE", inoculated_. == 25)[,6:17]
PSME_TSHE_25 <- filter(alive, soil_species == "PSME",seed_species == "TSHE", inoculated_. == 25)[,6:17]
TSHE_PSME_25 <- filter(alive, soil_species == "TSHE",seed_species == "PSME", inoculated_. == 25)[,6:17]
THPL_PSME_25 <- filter(alive, soil_species == "THPL",seed_species == "PSME", inoculated_. == 25)[,6:17]
PSME_PSME_25 <- filter(alive, soil_species == "PSME",seed_species == "PSME", inoculated_. == 25)[,6:17]

filter(alive, soil_species == "TSHE",seed_species == "TSHE", inoculated_. == 25)
filter(alive, soil_species == "TSHE", seed_species == "PSME", inoculated_. == 25)
treatment_list <- list(
  TSHE_TSHE_0,
  THPL_TSHE_0,
  PSME_TSHE_0,
  TSHE_PSME_0,
  THPL_PSME_0,
  PSME_PSME_0,
  
  TSHE_TSHE_10,
  THPL_TSHE_10,
  PSME_TSHE_10,
  TSHE_PSME_10,
  THPL_PSME_10,
  PSME_PSME_10,
  
  TSHE_TSHE_25,
  THPL_TSHE_25,
  PSME_TSHE_25,
  TSHE_PSME_25,
  THPL_PSME_25,
  PSME_PSME_25 
)
list_of_treatment_names<- c(
"TSHE_TSHE_0",
"THPL_TSHE_0",
"PSME_TSHE_0",
"TSHE_PSME_0",
"THPL_PSME_0",
"PSME_PSME_0",

"TSHE_TSHE_10",
"THPL_TSHE_10",
"PSME_TSHE_10",
"TSHE_PSME_10",
"THPL_PSME_10",
"PSME_PSME_10",

"TSHE_TSHE_25",
"THPL_TSHE_25",
"PSME_TSHE_25",
"TSHE_PSME_25",
"THPL_PSME_25",
"PSME_PSME_25"
)

# Calculates sums of total germination percent 

# get_sums <- function(df){
#   unname(colSums(df)
# }

get_max_percent <- function(df){
  max(colMeans(df))/10
}


##### List of all treatments and their max germination percent #####

max_percents <- data.frame(
  percent = sapply(treatment_list,get_max_percent),
  treatment = list_of_treatment_names)

mean(max_percents$percent)


sapply(treatment_list,get_max_percent)

max_germination <- sapply(treatment_list, get_max_percent(df))
get_max_percent(THPL_TSHE_25)


max_germination(treatment_list)
get_sums(TSHE_alive)/1050
"Compared to purity of 86"
colSums(PSME_alive)/1050
"Compared to a purity of 85"



# Calculates the percent germinated each day based on a given dataframe and total number of seeds in treatment
percent_germ_per_day <- function(df,total_seeds){
  a1 <- data.frame(doy = days_of_experiment, germ_percent = get_sums(df)/total_seeds)
  return(a1)
}

percent_germ_per_day(TSHE_alive,1050)

germ_chart <- function(df,total_seeds){
ggplot(data = percent_germ_per_day(df,total_seeds), aes(x=percent_germ_per_day(df,total_seeds)$doy,
                                            y=percent_germ_per_day(df,total_seeds)$germ_percent,
                                            group=1)) + 
    geom_line() +
    geom_point()
}

# Check germination percent of TSHE and PSME separately 
germ_chart(TSHE_alive,1050)
germ_chart(PSME_alive,1050)

germ_chart(filter(alive,seed_species == "TSHE",soil_species == "PSME")[,6:17],1050/3)
germ_chart(filter(alive,seed_species == "TSHE", soil_species == "TSHE")[,6:17],1050/3)

TSHE_germination <- data.frame(
  percent_germ = get_sums(TSHE_alive)/1050,
  doy = days_of_experiment
)

ggplot(data = TSHE_germination, aes(x = doy,y=percent_germ,group=1)) + 
  geom_line() + 
  geom_point()
