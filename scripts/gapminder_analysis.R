library(tidyverse)

# Load gapminder csv file
gapminder <- read.csv("data/gapminder_data.csv")

str(gapminder)

summary(gapminder)

colnames(gapminder)

year_country_gdp <- select(gapminder, year, country, gdpPercap)
year_country_gdp

smaller_gapminder_data <- select(gapminder, -continent)

smaller_gapminder_data <- gapminder %>% select(-continent)

year_country_gdp <- gapminder %>% select(year, country, gdpPercap)

tidy_gdp <- year_country_gdp %>% rename(gdp_per_capita = gdpPercap)

# Filtering with dplyr

year_country_gdp_euro <- gapminder %>%
  filter(continent == "Europe", year == 2007) %>%
  select(country, lifeExp)

year_country_gdp_africa <- gapminder %>%
  filter(continent == "Africa", year != 2007) %>%
  select(country, lifeExp, year)

#year_country_gdp_africa <- select(filter(gapminder, continent == "Africa", year != 2007), country, lifeExp, year)

str(gapminder %>% group_by(continent))

gapminder_grouped_cont <- gapminder %>% group_by(continent)

gdp_bycontinents <- gapminder_grouped_cont %>%
  summarize(mean_gdpPercap = mean(gdpPercap))

gdp_bycontinents_byyear <- gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap))

gdp_pop_bycontinents_byyears <- gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop))

# Using mutate, groupby, & summarize
gdp_pop_bycontinents_byyears <- gapminder %>%
  mutate(gdp_billion = gdpPercap*pop/10^9) %>%
  group_by(continent, year) %>%
  summarize(mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))

# Connect mutate with logical filtering
gdp_cont_year_above30 <- gapminder %>%
  mutate(gdp_billion = ifelse(lifeExp > 30, 
                              gdpPercap * pop / 10^9, 
                              NA)) %>%
  group_by(continent, year) %>%
  summarize(mean_gdp_billion = mean(gdp_billion))

# Combining dplyr & ggplot2
gapminder %>%
  # Filter countries in Asia
  filter(continent == "Asia") %>%
  # Make the plot
  ggplot(mapping = aes(x = year,
                       y = lifeExp)) +
  geom_line() +
  facet_wrap( ~ country) +
  theme(axis.text.x = element_text(angle = 45))

gapminder %>%
  # extract first of country name into column
  mutate(startsWith = substr(country, 1, 1)) %>%
  # only keep countries that start with A & Z
  filter(startsWith %in% c("A", "Z")) %>%
  # plot life expectancy into facets
  ggplot(aes(x = year,
             y = lifeExp,
             colour = continent)) +
  geom_line() +
  facet_wrap(vars(country)) + 
  theme_minimal()




