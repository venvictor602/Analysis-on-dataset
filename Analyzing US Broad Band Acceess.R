library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())

tt <- tt_load("2021-05-11")

broadband_county <-  tt$broadband %>% 
  janitor::clean_names() %>% 
  rename(state = st) %>% 
  mutate(state = state.name[match(state, state.abb)] ) %>% 
  mutate(state = ifelse(is.na(state), "distribution Columbia", state),
         broadband_availability_per_fcc = parse_number(broadband_availability_per_fcc, na = "-"),
         broadband_usage = parse_number(broadband_usage, na = "-")) %>% 
  mutate(county = paste0(str_remove(county_name, "County$"), ", ", state),
         county = fct_reorder(county, broadband_availability_per_fcc))  # county$ means county from the end

broadband_county %>% 
  view()

boradband_zip <- tt$broadband_zip %>% 
  janitor::clean_names() %>% 
  rename(state = st)


#get the US population by county from the link to join the 
# poulation of the people in a county by the county data set
#https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html 
# download the us own on the first ones that comes up

install.packages("readxl")
library(readxl)


county_population <- read_excel("C:/Users/USER/Downloads/co-est2021-pop.xlsx", skip = 3) %>% 
  select(county = ...1, population_2021 = "2021") %>% 
  separate(county, c("county_name", "state"), sep = ", ") %>% 
  mutate(county_name = str_remove(county_name, "^\\."))#this remove the  . infront of hte county name 
# joining the two data set of broadband and county population

broadbnd_with_population <- broadband_county %>% 
  inner_join(county_population, by = c("county_name", "state") )  

broadbnd_with_population %>% 
  arrange(desc(population_2021)) %>% 
  head(40) %>% 
  mutate(county = paste0(str_remove(county_name, "County$"), ", ", state),
         county = fct_reorder(county, broadband_availability_per_fcc)) %>% # county$ means county from the end
  ggplot(aes(broadband_availability_per_fcc,  county ))+
  geom_point()+
  scale_x_continuous(labels = percent_format())
  
broadbnd_with_population %>% 
  arrange(desc(population_2021)) %>% 
  filter(population_2021 >= 30000) %>% 
  ggplot(aes(population_2021, broadband_availability_per_fcc))+
  geom_point()+
  geom_text(aes(label = county), check_overlap = TRUE, vjust = 1, hjust = 1)+
  scale_x_log10(labels = comma_format())+
  scale_y_continuous(labels = percent_format())+
  expand_limits(x = 10000)+
  labs(y  = "broadband availability in 2022",
       x = "population in 2021")

#taking a look at the broad band usage 

broadbnd_with_population %>% 
  arrange(desc(population_2021)) %>% 
  filter(population_2021 >= 30000) %>% 
  ggplot(aes(population_2021, broadband_usage))+
  geom_point()+
  geom_text(aes(label = county), check_overlap = TRUE, vjust = 1, hjust = 1)+
  scale_x_log10(labels = comma_format())+
  scale_y_continuous(labels = percent_format())+
  expand_limits(x = 10000)+
  labs(y  = "broadband usage in 2022",
       x = "population in 2021")
