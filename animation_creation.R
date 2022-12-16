
## Script to create a gif depicting abuse against women over time

# Load libraries ---------------------------------------------------------------

library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(stringr)
library(ggtext)
library(kableExtra)
library(gridExtra)

# Data setup -------------------------------------------------------------------
# Clear environment
rm(list = ls())

setwd("/Users/coletteyeager/Documents/Documents /Georgetown/Fall 2022/Data Viz/PPOL563_Final_Project")

# Read in data
df <- read.csv("Project data/women_violence.csv")

old_long_name = colnames(df)[4]
df <- df %>%
  rename("Country" = "Entity", "Proportion_Violence" = all_of(old_long_name)) %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  select(!c("Code"))

# Countries with regulations against domestic violence
countries <- c('Antigua and Barbuda', 'Argentina', 'Bahamas', 'Barbados', 'Belize', 
             'Bolivia', 'Brazil', 'Chile', 'Colombia', 'Costa Rica', 'Dominica', 
             'Dominican Republic', 'Ecuador', 'El Salvador', 'Guatemala', 'Guyana', 
             'Honduras', 'Jamaica', 'Mexico', 'Nicaragua', 'Panema', 'Paraguay', 
             'Peru', 'Puerto Rico', 'Saint Lucia', 'Saint Vincent and the Grenadines', 
             'Trinidad and Tobago', 'Uruguay')
year_passed <- c(1999, 1994, 1991, 1992, 1992, 1995, 1995, 1994, 1996, 1996, 
                 1996, 1997, 1995, 1996, 1999, 1996, 1997, 1996, 1996, 1996, 
                 1999, 2000, 1993, 1989, 1995, 1984, 1999, 1995)
laws_passed <- data.frame(countries, year_passed)
colnames(laws_passed) <- c("Country", "year_law_passed")

sample_of_countries <- laws_passed %>%
  filter(Country %in% c('Bahamas', 'Dominican Republic', 'El Salvador',
                        'Honduras', 'Nicaragua', 'Peru'))

wrap_text <- function(x, chars = 10) {
  stringr::str_wrap(x, chars)
}

# Animation --------------------------------------------------------------------

p1 <- 
  df %>%
  inner_join(laws_passed, by = "Country") %>%
  filter(Country %in% c('Bahamas', 'Dominican Republic', 'El Salvador',
                        'Honduras', 'Nicaragua', 'Peru')) %>%
  ggplot(aes(x = Year, y = Proportion_Violence)) +
  geom_line() +
  facet_wrap(~Country, scales = 'free_y', 
             nrow = 2, labeller = as_labeller(wrap_text)) +
  geom_vline(data = sample_of_countries, aes(xintercept = year_law_passed), color = 'blue') +
  geom_text(data = sample_of_countries, aes(x = year_law_passed, label = year_law_passed,
                                            y = -Inf), hjust = -0.1, 
            vjust = -0.9, angle = 90, size = 3) +
  labs(x = "Year", y = "Proportion of Women Abused") +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  xlim(1988, 2020) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_text(size = 15)) +
  geom_point() +
  transition_reveal(Year)

# Save animation
anim_save("www/over_time.gif", animate(p1, fps = 15, end_pause = 25), width = 1200, height = 700, units = "px")
