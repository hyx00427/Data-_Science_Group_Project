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

youth_NEET <- read.csv("youth-not-in-education-employment-training.csv",
                                   sep = ",",
                                   stringsAsFactors = F)

# left join the GDP_per_capita_world by the countries_in_education data frame
left_join_GDP_world <- GDP_per_capita_world %>%
  left_join(countries_in_continents,
            join_by("Entity" == "Entity", "Code" == "Code")) %>%
  rename_with(~ "GDP_per_capita", .cols = 4) %>% # rename the GDP per capita column to make it easier to work with
  mutate(GDP_growth = (GDP_per_capita - lag(GDP_per_capita)) * 100/ GDP_per_capita)

# group the GDP values by continents and find the evolution of mean GDP for each continent
continent_gdp_evolution <- left_join_GDP_world %>%
  filter(!is.na(Continent)) %>%    # filter N/A values from the continents column
  group_by(Continent, Year.x) %>%
  summarise(mean_GDP_growth = mean(GDP_growth, na.rm = TRUE))

# plot the evolution of mean GDP for each continent
ggplot(continent_gdp_evolution, aes(x = Year.x,
                                    y = mean_GDP_growth,
                                    color = Continent)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Africa" = "red",
                                "Asia" = "darkgreen",
                                "Europe" = "gold",
                                "North America" = "green",
                                "Oceania" = "pink",
                                "South America" = "purple")) +
  labs(x = "Year",
       y = "Percentage growth of GDP per capita (%)",
       title = "Average growth of GDP per capita globally",
       color = "Continent") +
  scale_x_continuous(limits = c(1991,2021)) +
  scale_y_continuous(limits = c(-50, 25)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"))

