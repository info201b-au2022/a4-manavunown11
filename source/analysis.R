library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
# The functions might be useful for A4
source("/Users/manav/Documents/info201/assignments/a4-manavunown11/source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
incarceration_data <- read.csv("~/Documents/info201/data/incarceration_trends.csv")

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function creates a data frame of prison population in the US from 1970-2018
get_year_jail_pop <- function() {
  sum_prison_pop <- incarceration_data %>% select(year, total_jail_pop)
return(sum_prison_pop)   
}

# This function plots a bar chart showing a visualization of the data collected in the function above 
plot_jail_pop_for_us <- function()  {
  jail_pop <- get_year_jail_pop()
  graph <- ggplot(data = jail_pop) + geom_col(mapping = aes(x= year, y = total_jail_pop)) + 
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", x = "Year", y = "Total Jail Population")
  return(graph)   
} 
plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states){
  state_prison_pop <- incarceration_data %>% filter(is.element(state, states) & !is.na(total_jail_pop)) %>% group_by(year, state) %>% 
    summarise(total_jail_pop = sum(total_jail_pop))
  return(state_prison_pop)
}
get_jail_pop_by_states(c("WA", "OR", "CA"))

plot_jail_pop_by_states <- function(states){
  state_data <- get_jail_pop_by_states(states)
  plot_state_prison_pop <- ggplot(state_data, mapping = aes(x = year, y = total_jail_pop, group = state, color = state)) + geom_line() +
    labs(title = "Line Chart to plot variation", x = "Year", y = "Jail Population by state")
  return(plot_state_prison_pop)
}
plot_jail_pop_by_states(c("WA", "OR", "CA"))

plot_jail_pop_by_states <- function(states){
  state_data <- get_jail_pop_by_states(states)
  plot_state_prison_pop <- ggplot(state_data, mapping = aes(x = year, y = total_jail_pop, group = state, color = state)) + geom_line() +
    labs(title = "Line Chart to plot variation", x = "Year", y = "Jail Population by state")
    return(plot_state_prison_pop)
}
plot_jail_pop_by_states(c("WA", "OR", "CA", "NY", "WV", "OH", "TX", "FL"))
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

black_pop_info_df <- incarceration_data %>% select(yfips, year, fips, state, county_name, total_pop, black_pop_15to64,total_jail_pop, total_prison_pop, 
                                          black_prison_pop, black_jail_pop, black_male_prison_pop, black_female_prison_pop, black_jail_pop_rate) %>% 
  mutate("county_location" = paste0(county_name, ", ", state), "black_pop_prop" = paste0((black_pop_15to64/total_pop) * 100))

highest_jail_black_county <- black_pop_info_df %>% filter(year == 2016) %>% select(year, county_location, black_jail_pop) %>% 
  arrange(desc(black_jail_pop)) %>% slice_head() %>% pull(county_location)

highest_pop_black_county <- black_pop_info_df %>% filter(year == 2016) %>% filter(black_pop_15to64 == max(black_pop_15to64)) %>% pull(county_location)
highest_pop_prop_black_county <- black_pop_info_df %>% filter(year == 2016) %>% filter(black_pop_prop == max(black_pop_prop)) %>% pull(county_location)
highest_jail_black_rate_county <- black_pop_info_df %>% filter(year == 2016) %>% select(year, county_location, black_jail_pop_rate) %>% 
  arrange(desc(black_jail_pop_rate)) %>% slice_head() %>% pull(county_location)

get_black_pop_prison <- function(){
  black_pop_prison_df <- black_pop_info_df %>% select(year, county_location, black_pop_15to64, black_jail_pop, black_pop_prop, black_jail_pop_rate) %>% filter(county_location == highest_pop_black_county)
  return(black_pop_prison_df)
}

plot_black_pop_prison <- function(){
  data <- get_black_pop_prison()
  black_pop_prison_graph <- ggplot(data) +
  geom_col(mapping = aes(x = black_pop_prop, y = black_jail_pop)) +
  labs(title = "Black Population vs Jail Population", x = "Black Population", y = "Black Jail Population")
  return(black_pop_prison_graph)
}
plot_black_pop_prison()
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
map_df <- black_pop_info_df %>% filter(year == 2016) %>% select(fips, county_name, black_jail_pop_rate) %>% filter(!is.na(black_jail_pop_rate))
black_county_pop_df <- map_data("county") %>% unite(polyname, region, subregion, sep = ",") %>% left_join(county.fips, by = "polyname")
  
get_black_pop_prison_map <- function(){
  black_pop_prison_map_df <- black_county_pop_df %>% left_join(map_df, by = "fips")
  return(black_pop_prison_map_df)
}

blank_theme <- theme_bw() +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

black_prison_pop_map <- function(){
  black_map_df <- get_black_pop_prison_map()
  latest_black_jail_pop_rate <- ggplot(black_map_df) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
                 color = "black", size = 0.2) + scale_fill_continuous(limits = c(0, max(black_map_df$black_jail_pop_rate)), 
                          na.value = "white", low = "green", high = "red") +
    blank_theme +
    labs(title = "Latest Black Jail Population Rate in the USA")
  return(latest_black_jail_pop_rate)
}
black_prison_pop_map()
## Load data frame ---- 


