library("tidyverse")
library("dplyr")
library("ggplot2")

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
two_decades_incarceration <- incarceration_trends[incarceration_trends$year >= 1995 & incarceration_trends$year <= 2015, ] %>%
  mutate(total_aapi_pop = aapi_jail_pop + aapi_prison_pop) %>%
  mutate(total_black_pop = black_jail_pop + black_prison_pop) %>%
  mutate(total_latinx_pop = latinx_jail_pop + latinx_prison_pop) %>%
  mutate(total_native_pop = native_jail_pop + native_prison_pop) %>%
  mutate(total_white_pop = white_jail_pop + white_prison_pop) %>%
  mutate(total_prison_jail_pop = total_jail_pop + total_prison_pop) %>%
  mutate(aapi_prop = total_aapi_pop / total_prison_jail_pop) %>%
  mutate(black_prop = total_black_pop / total_prison_jail_pop) %>%
  mutate(latinx_prop = total_latinx_pop / total_prison_jail_pop) %>%
  mutate(native_prop = total_native_pop / total_prison_jail_pop) %>%
  mutate(white_prop = total_white_pop / total_prison_jail_pop)
  
#Summary Information
#1. What is the total jail + prison population for minorities (AAPI, African American, Latinx, Native Americans) in the US since 1995?
total_minority_pop_num <- sum(two_decades_incarceration$total_aapi_pop, na.rm = TRUE) + sum(two_decades_incarceration$total_black_pop, na.rm = TRUE) +
  sum(two_decades_incarceration$total_latinx_pop, na.rm = TRUE) + sum(two_decades_incarceration$total_native_pop, na.rm = TRUE)

#2. What is the total jail + prison population for Whites in the US since 1995?
total_white_pop_num <- sum(two_decades_incarceration$total_white_pop, na.rm = TRUE)

#3. What is the proportion of minorities in jails and prisons combined?
minority_prop_num <- total_minority_pop_num / sum(two_decades_incarceration$total_prison_jail_pop, na.rm = TRUE)

#4. What is the proportion of Whites in jails and prisons combined?
white_prop_num <- total_white_pop_num / sum(two_decades_incarceration$total_prison_jail_pop, na.rm = TRUE)

#5. In what region of the US was the proportion of minorities in jails and prisons combined highest?
region_based <- two_decades_incarceration %>%
  group_by(region) %>%
  summarise(total_aapi_pop_jp = sum(total_aapi_pop, na.rm = TRUE), 
            total_black_pop_jp = sum(total_black_pop, na.rm = TRUE), 
            total_latinx_pop_jp = sum(total_latinx_pop, na.rm = TRUE), 
            total_native_pop_jp = sum(total_native_pop, na.rm = TRUE), 
            total_white_pop_jp = sum(total_white_pop, na.rm = TRUE),
            total_jp_pop = sum(total_prison_jail_pop, na.rm = TRUE)) %>%
  mutate(total_minority_pop_jp = total_aapi_pop_jp + total_black_pop_jp + total_latinx_pop_jp + total_native_pop_jp) %>%
  mutate(minority_prop = total_minority_pop_jp / total_jp_pop)
highest_minority_prop_region <- toString(region_based[region_based$minority_prop == max(region_based$minority_prop), "region"])

#6. In what region of the US was the proportion of Whites in jails and prisons combined highest?
region_based <- region_based %>%
  mutate(white_prop = total_white_pop_jp / total_jp_pop)
highest_white_prop_region <- toString(region_based[region_based$white_prop == max(region_based$white_prop), "region"])


#Trend Chart
race_pop_by_year <- two_decades_incarceration %>%
  group_by(year) %>%
  summarise(total_aapi_pop_jp = sum(total_aapi_pop, na.rm = TRUE), 
            total_black_pop_jp = sum(total_black_pop, na.rm = TRUE), 
            total_latinx_pop_jp = sum(total_latinx_pop, na.rm = TRUE), 
            total_native_pop_jp = sum(total_native_pop, na.rm = TRUE), 
            total_white_pop_jp = sum(total_white_pop, na.rm = TRUE))

options(scipen = 1000000)
trend_chart <- ggplot(data = race_pop_by_year, aes(x = year, color = group)) +
  geom_line(aes(y = total_aapi_pop_tc, color = "red")) + 
  geom_line(aes(y = total_black_pop_tc, color = "yellow")) + 
  geom_line(aes(y = total_latinx_pop_tc, color = "purple")) + 
  geom_line(aes(y = total_native_pop_tc, color = "green")) + 
  geom_line(aes(y = total_white_pop_tc, color = "blue")) +
  labs(x = "Year", y = "Total Prison + Jail Population") +
  scale_color_manual(name = "Race", labels = c("White", "Native", "Latinx", "AAPI", "Black"), 
                     values = c("blue", "green", "purple", "red", "black")) +
  ggtitle("Change in Prison + Jail Populations of Each Race from 1995 to 2015")

#Variable Comparison Chart
white_vs_minority_races <- two_decades_incarceration %>%
  group_by(year) %>%
  select(year, total_aapi_pop, total_black_pop, total_latinx_pop, total_white_pop, 
         total_native_pop, total_jail_pop, total_prison_pop, total_prison_jail_pop) %>%
  summarise(total_aapi_pop = sum(total_aapi_pop, na.rm = TRUE),
            total_black_pop = sum(total_black_pop, na.rm = TRUE),
            total_latinx_pop = sum(total_latinx_pop, na.rm = TRUE),
            total_native_pop = sum(total_native_pop, na.rm = TRUE),
            total_white_pop = sum(total_white_pop, na.rm = TRUE),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
            total_prison_jail_pop = sum(total_prison_jail_pop, na.rm = TRUE)) %>%
  mutate(aapi_prop = total_aapi_pop / total_prison_jail_pop) %>%
  mutate(black_prop = total_black_pop / total_prison_jail_pop) %>%
  mutate(latinx_prop = total_latinx_pop / total_prison_jail_pop) %>%
  mutate(native_prop = total_native_pop / total_prison_jail_pop) %>%
  mutate(white_prop = total_white_pop / total_prison_jail_pop)

var_com_chart <- ggplot(data = white_vs_minority_races, aes(x = white_prop, y = black_prop)) +
  geom_point(shape = 18) +
  labs(y = "Proportion of Blacks in Prisons + Jails", x = "Proportion of Whites in Prisons + Jails") +
  ggtitle("Proportion of Non-White Races in Prisons + Jails Relative to Whites from 1995 to 2015")

#Variable comparisons chart: prison + jail population proportion of minority groups vs whites