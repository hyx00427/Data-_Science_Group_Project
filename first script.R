library(tidyverse)
library(ggplot2)

setwd("~/YEAR 1/Autumn Term/Intro to Data Science/group assignment 2025")

# making the data frames
countries_in_continents <- read.csv("continents-according-to-our-world-in-data.csv",
                                    sep = ",",
                                    stringsAsFactors = F)

GDP_per_capita_world <- read.csv("gdp-per-capita-worldbank.csv",
                                 sep = ",",
                                 stringsAsFactors = F)

youth_not_in_education <- read.csv("youth-not-in-education-employment-training.csv",
                                   sep = ",",
                                   stringsAsFactors = F)