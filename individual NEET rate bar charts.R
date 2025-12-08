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

# compute average neet
continent_NEET_evolution <- left_join_NEET_world %>%
  filter(!is.na(Continent)) %>%
  group_by(Continent, Year.x) %>%
  summarise(mean_NEET = mean(NEET_rate, na.rm = TRUE), .groups="drop")

#-----------------------------------------------------------------
# Filter for Africa only
africa_NEET <- continent_NEET_evolution %>%
  filter(Continent == "Africa")

# filter years 2010, 2015 and 2020 from data set, and categorise by year
africa_key_years <- continent_NEET_evolution %>%
  filter(Continent == "Africa" & Year.x %in% c(2010, 2015, 2020)) %>%
  mutate(Year.x = as.factor(Year.x))

# plot
ggplot(africa_key_years,
       aes(x = Year.x,
           y = mean_NEET,
           fill = Year.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(mean_NEET, 1)),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("2010" = "gold",
                               "2015" = "darkorange",
                               "2020" = "red")) +
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Youth NEET Rate in Africa: 2010, 2015, 2020",
       fill = "Year") +
  scale_y_continuous(limits = c(0,32)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-----------------------------------------------------------------
# Filter for Asia only
asia_NEET <- continent_NEET_evolution %>%
  filter(Continent == "Asia")

# filter years 2010, 2015 and 2020 from data set, and categorise by year
asia_key_years <- continent_NEET_evolution %>%
  filter(Continent == "Asia" & Year.x %in% c(2010, 2015, 2020)) %>%
  mutate(Year.x = as.factor(Year.x))

# plot
ggplot(asia_key_years,
       aes(x = Year.x,
           y = mean_NEET,
           fill = Year.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(mean_NEET, 1)),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("2010" = "lightblue",
                               "2015" = "blue",
                               "2020" = "darkblue")) +
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Youth NEET Rate in Asia: 2010, 2015, 2020",
       fill = "Year") +
  scale_y_continuous(limits = c(0,32)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-----------------------------------------------------------------
# Filter for Europe only
europe_NEET <- continent_NEET_evolution %>%
  filter(Continent == "Europe")

# filter years 2010, 2015 and 2020 from data set, and categorise by year
europe_key_years <- continent_NEET_evolution %>%
  filter(Continent == "Europe" & Year.x %in% c(2010, 2015, 2020)) %>%
  mutate(Year.x = as.factor(Year.x))

# plot
ggplot(europe_key_years,
       aes(x = Year.x,
           y = mean_NEET,
           fill = Year.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(mean_NEET, 1)),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("2010" = "pink",
                               "2015" = "magenta",
                               "2020" = "purple")) +
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Youth NEET Rate in Europe: 2010, 2015, 2020",
       fill = "Year") +
  scale_y_continuous(limits = c(0,32)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-----------------------------------------------------------------
# Filter for North America only
NA_NEET <- continent_NEET_evolution %>%
  filter(Continent == "North America")

# filter years 2010, 2015 and 2020 from data set, and categorise by year
NA_key_years <- continent_NEET_evolution %>%
  filter(Continent == "North America" & Year.x %in% c(2010, 2015, 2020)) %>%
  mutate(Year.x = as.factor(Year.x))

# plot
ggplot(NA_key_years,
       aes(x = Year.x,
           y = mean_NEET,
           fill = Year.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(mean_NEET, 1)),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("2010" = "#B2EBF2",
                               "2015" = "green",
                               "2020" = "darkgreen")) +
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Youth NEET Rate in North America: 2010, 2015, 2020",
       fill = "Year") +
  scale_y_continuous(limits = c(0,32)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-----------------------------------------------------------------
# Filter for South America only
SA_NEET <- continent_NEET_evolution %>%
  filter(Continent == "South America")

# filter years 2010, 2015 and 2020 from data set, and categorise by year
SA_key_years <- continent_NEET_evolution %>%
  filter(Continent == "South America" & Year.x %in% c(2010, 2015, 2020)) %>%
  mutate(Year.x = as.factor(Year.x))

# plot
ggplot(SA_key_years,
       aes(x = Year.x,
           y = mean_NEET,
           fill = Year.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(mean_NEET, 1)),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("2010" = "purple",
                               "2015" = "blue",
                               "2020" = "darkgreen")) +
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Youth NEET Rate in South America: 2010, 2015, 2020",
       fill = "Year") +
  scale_y_continuous(limits = c(0,32)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-----------------------------------------------------------------
# Filter for Oceania only
oceania_NEET <- continent_NEET_evolution %>%
  filter(Continent == "Oceania")

# filter years 2010, 2015 and 2020 from data set, and categorise by year
oceania_key_years <- continent_NEET_evolution %>%
  filter(Continent == "Oceania" & Year.x %in% c(2010, 2015, 2020)) %>%
  mutate(Year.x = as.factor(Year.x))

# plot
ggplot(oceania_key_years,
       aes(x = Year.x,
           y = mean_NEET,
           fill = Year.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(mean_NEET, 1)),
            vjust = -0.5,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("2010" = "red",
                               "2015" = "blue",
                               "2020" = "green")) +
  labs(x = "Year",
       y = "Average NEET Rate (%)",
       title = "Youth NEET Rate in Oceania: 2010, 2015, 2020",
       fill = "Year") +
  scale_y_continuous(limits = c(0,35)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
