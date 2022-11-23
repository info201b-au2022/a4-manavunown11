library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
# The functions might be useful for A4
source("/Users/manav/Documents/info201/assignments/a4-manavunown11/source/a4-helpers.R")
incarceration_data <- read.csv("~/Documents/info201/data/incarceration_trends.csv")
## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
#Calculating population for black people in jail in 2016
black_jail_pop_2016 <- incarceration_data %>% select(year, black_jail_pop) %>% filter(year == 2016) %>%
  summarize(black_pop_jailed = sum(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_pop_jailed)

jail_pop_2016_total <- incarceration_data %>% select(year, total_jail_pop) %>% filter(year == 2016) %>%
  summarize(total_pop_jail = sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(total_pop_jail)
#Calculating percentage of black people in jail in 2016
black_jail_percent = (black_jail_pop_2016/jail_pop_2016_total)* 100

#Average population of black people over recorded time period
black_pop_sum <- incarceration_data %>% summarize(black_sum = sum(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_sum)

black_pop_avg = black_pop_sum/(2018 - 1970)
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
#Function creates a dataframe to containing US Prison Data from 1970-2018
get_jail_pop_by_states <- function(states){
  state_prison_pop <- incarceration_data %>% filter(is.element(state, states) & !is.na(total_jail_pop)) %>% group_by(year, state) %>% 
    summarise(total_jail_pop = sum(total_jail_pop))
  return(state_prison_pop)
}
get_jail_pop_by_states(c("WA", "OR", "CA"))

#These functions plot the data from the above function showing multiple states
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
    labs(title = "Line Chart to plot variation by states", x = "Year", y = "Jail Population by state")
    return(plot_state_prison_pop)
}
plot_jail_pop_by_states(c("WA", "OR", "CA", "NY", "WV", "OH", "TX", "FL"))
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#Creating dataframe with all data related to black populations
black_pop_info_df <- incarceration_data %>% select(yfips, year, fips, state, county_name, total_pop, 
                                                   black_pop_15to64,total_jail_pop, total_prison_pop, 
                                          black_prison_pop, black_jail_pop, black_male_prison_pop, 
                                          black_female_prison_pop, black_jail_pop_rate) %>% 
  mutate("county_location" = paste0(county_name, ", ", state), 
         "black_pop_prop" = paste0((black_pop_15to64/total_pop) * 100))

#Calculating various statistics about counties relating to black populations
highest_jail_black_county <- black_pop_info_df %>% filter(year == 2016) %>% 
  select(year, county_location, black_jail_pop) %>% 
  arrange(desc(black_jail_pop)) %>% slice_head() %>% pull(county_location)
highest_pop_black_county <- black_pop_info_df %>% filter(year == 2016) %>% 
  filter(black_pop_15to64 == max(black_pop_15to64)) %>% pull(county_location)
highest_pop_prop_black_county <- black_pop_info_df %>% filter(year == 2016) %>% 
  filter(black_pop_prop == max(black_pop_prop)) %>% pull(county_location)
highest_jail_black_rate_county <- black_pop_info_df %>% filter(year == 2016) %>% 
  select(year, county_location, black_jail_pop_rate) %>% 
  arrange(desc(black_jail_pop_rate)) %>% slice_head() %>% pull(county_location)

king_tx_black_pop_prop <- black_pop_info_df %>% filter(year == 2016) %>% filter(county_location == "King County, TX") %>%
  pull(black_pop_prop)

#Creating dataframe with all data related to plotting the graph with two continuous variables
get_black_pop_prison <- function(){
  black_pop_prison_df <- black_pop_info_df %>% 
    select(year, county_location, black_pop_15to64, black_jail_pop) %>% filter(county_location == "King County, TX") %>%
    filter(!is.na(black_pop_15to64) & !is.na(black_jail_pop)) 
  return(black_pop_prison_df)
}

#Plots a graph of black population proportion in King County, TX with respect to jailing
get_black_pop_prison_graph <- function(){
  black_pop_prison_plot <- ggplot(get_black_pop_prison()) +
    geom_smooth(aes(x = black_pop_15to64, y = black_jail_pop)) +
    labs(title = "Black Population vs Prison Population", x = "Black Population", y = "Black Jail Population")
  return(black_pop_prison_plot)
}
get_black_pop_prison_graph()
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#Wrangling data required to map the jail rate as in the next function
get_black_pop_prison_map <- function(){
  map_df <- black_pop_info_df %>% filter(year == 2016) %>% select(fips, county_name, black_jail_pop_rate) %>% 
  filter(!is.na(black_jail_pop_rate))
  black_county_pop_df <- map_data("county") %>% unite(polyname, region, subregion, sep = ",") %>% left_join(county.fips, by = "polyname")
  black_pop_prison_map_df <- black_county_pop_df %>% left_join(map_df, by = "fips")
  return(black_pop_prison_map_df)
}

#Plotting map of counties in the US based on their Jail Population rates

black_prison_pop_map <- function(){
  blank_theme <- theme_bw() +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())
  black_map_df <- get_black_pop_prison_map()
  latest_black_jail_pop_rate <- ggplot(black_map_df) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
                 color = "white", size = 0.1) + scale_fill_continuous(limits = c(0, max(black_map_df$black_jail_pop_rate)), 
                          na.value = "white", low = "blue", high = "red") + blank_theme +
    labs(title = "Latest Black Jail Population Rate in the USA")
  return(latest_black_jail_pop_rate)
}
black_prison_pop_map()



