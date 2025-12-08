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

# rename column
youth_NEET <- youth_NEET %>%
  rename_with(~ "NEET_rate", .cols = 4)

# join data
left_join_NEET_world <- youth_NEET %>%
  left_join(countries_in_continents,
            join_by("Entity" == "Entity", "Code" == "Code"))

# --------------------------------------------------------------
# compute average neet
continent_NEET_evolution <- left_join_NEET_world %>%
  filter(!is.na(Continent)) %>%
  group_by(Continent, Year.x) %>%
  summarise(mean_NEET = mean(NEET_rate, na.rm = TRUE), .groups="drop")

# LINE GRAPH OF MEANS BY CONTINENT
ggplot(continent_NEET_evolution,
                 aes(x = Year.x,
                     y = mean_NEET,
                     color = Continent)) +
  geom_line(linewidth = 0.6) +
  
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Evolution of Average Youth NEET Rates by Continent",
       color = "Continent") +
  scale_x_continuous(limits = c(1990,2021)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#-----------------------------------------------------------------
# Filter for Africa only
africa_NEET <- continent_NEET_evolution %>%
  filter(Continent == "Africa")

# Plot Africa alone - LINE GRAPH
ggplot(africa_NEET,
       aes(x = Year.x,
           y = mean_NEET)) +
  geom_line(color = "red",
            linewidth = 1) +
  geom_point(size = 1.5) +
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Evolution of Average Youth NEET Rate in Africa",
       subtitle = "NEET = Not in Education, Employment, or Training") +
  scale_x_continuous(limits = c(1990, 2021)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

#-----------------------------------------------------------------

# 1. Filter and prepare data for key years
neet_key_years <- continent_NEET_evolution %>%
  filter(Year.x %in% c(2010, 2015, 2020)) %>%
  mutate(Year.x = as.factor(Year.x))  # Treat year as categorical for bar chart

# BAR CHART FOR MEAN NEET RATE BY CONTINENT
ggplot(neet_key_years,
       aes(x = Continent, 
           y = mean_NEET, 
           fill = Year.x)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8),
           width = 0.7) +
  labs(title = "Youth NEET Rates by Continent: 2010, 2015, 2020",
       x = "Continent",
       y = "Average NEET Rate (%)",
       fill = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

