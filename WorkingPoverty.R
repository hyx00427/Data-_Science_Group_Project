library(dplyr)
library(ggplot2)
library(tidyverse)

# Loading Datasets

continents <- read.csv("continents-according-to-our-world-in-data.csv")
gdp <- read.csv("gdp-per-capita-worldbank.csv")
neet <- read.csv("youth-not-in-education-employment-training.csv")
poverty <- read.csv("SDG_0111_SEX_AGE_RT_A-20251207T0233.csv")

# Renaming columns to more fitting column names

poverty <- poverty %>% 
  rename(working_poverty = obs_value) %>%
  rename(Year = time) %>%
  rename(Entity = ref_area.label)

neet <- neet %>%
  rename(neet_rate = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.)

gdp <- gdp %>%
  rename(gdp_per_capita = GDP.per.capita..PPP..constant.2017.international...)

# --- Merge all datasets using Entity + Year

data <- gdp %>%
  left_join(neet,    by = c("Entity", "Year")) %>%
  left_join(poverty, by = c("Entity", "Year")) %>%
  left_join(continents, by = "Entity")

# Removing all na data

clean <- data %>%
  filter(
    !is.na(working_poverty),
    !is.na(gdp_per_capita),
    !is.na(neet_rate),
    !is.na(Continent))

# 1. Working Poverty vs GDP

ggplot(clean, aes(x = gdp_per_capita, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs GDP", y = "Working Poverty", x = "GDP per Capita") +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

# 2. Working Poverty vs NEET

ggplot(clean, aes(x = neet_rate, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs NEET", y = "Working Poverty", x = "NEET rate") +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

# Each continents graph as having them all one was very clustered

clean_Africa <- data %>%
  filter(Continent == "Africa")


ggplot(clean_Africa, aes(x = gdp_per_capita, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs GDP in Africa", y = "Working Poverty", x = "GDP per capita") + scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

ggplot(clean_Africa, aes(x = neet_rate, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs NEET in Africa", y = "Working Poverty", x = "NEET rate") +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

clean_Asia <- data %>%
  filter(Continent == "Asia")

ggplot(clean_Asia, aes(x = gdp_per_capita, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs GDP in Asia", y = "Working Poverty", x = "GDP per capita") +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

ggplot(clean_Asia, aes(x = neet_rate, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs NEET in Asia", y = "Working Poverty", x = "NEET rate") +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))


clean_Europe <- data %>%
  filter(Continent == "Europe")

ggplot(clean_Europe, aes(x = gdp_per_capita, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs GDP in Europe", y = "Working Poverty", x = "GDP per capita") +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

ggplot(clean_Europe, aes(x = neet_rate, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs NEET in Europe", y = "Working Poverty", x = "NEET rate") +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))


clean_Oceania <- data %>%
  filter(Continent == "Oceania")

ggplot(clean_Oceania, aes(x = gdp_per_capita, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs GDP in Oceania", y = "Working Poverty", x = "GDP per capita") +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

ggplot(clean_Oceania, aes(x = neet_rate, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs NEET in Oceania", y = "Working Poverty", x = "NEET rate") +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))


clean_North_America <- data %>%
  filter(Continent == "North America")

ggplot(clean_North_America, aes(x = gdp_per_capita, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs GDP in North America", y = "Working Poverty", x = "GDP per Capita") +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

ggplot(clean_North_America, aes(x = neet_rate, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs NEET in North America", y = "Working Poverty", x = "NEET rate") +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))


clean_South_America <- data %>%
  filter(Continent == "South America")

ggplot(clean_South_America, aes(x = gdp_per_capita, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs GDP in South America", y = "Working Poverty", x = "GDP per capita") +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

ggplot(clean_South_America, aes(x = neet_rate, y = working_poverty, colour = Continent)) +
  geom_point() + labs(title = "Working Poverty vs NEET in South America", y = "Working Poverty", x = "NEET rate") +
  theme_minimal() +
  theme(plot.title = element_text(
    hjust = 0.5,
    size = 15,
    face = "bold"
  ))

