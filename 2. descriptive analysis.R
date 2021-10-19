# Descriptive investigation of movers
library(dplyr)
load(file = "global_mobility_eligible_researchers.RData")
countries_to_regions <- read.csv("countries_to_regions.csv")

global_mobility_eligible_researchers %>% distinct(cluster_id,.keep_all = T) %>% count(origin_country) %>% arrange(desc(n)) %>% View()
global_mobility_eligible_researchers %>% distinct(cluster_id,.keep_all = T) %>% count(origin_region) %>%  arrange(desc(n)) %>% View()


# Questions

# 1. For each region, how many people are moving, and where to?

destination_per_clusterid <- 
  global_mobility_eligible_researchers %>% 
  filter(pub_country != origin_country) %>% 
  group_by(cluster_id, pub_country) %>% 
  summarise(origin_country = first(origin_country), 
            origin_region = first(origin_region),
            gender = first(gender), 
            n = n(), 
            earliest_year =min(pub_year), 
            last_year = max(pub_year)) %>% 
  group_by(cluster_id) %>% 
  mutate(n_countries_moved_to = n()) %>% 
  ungroup() %>% 
  mutate(duration_years = last_year-earliest_year) %>% 
  left_join(countries_to_regions %>% rename(destination_region = region), by = c("pub_country" = "origin_country")) %>% 
  filter(n >1,
         duration_years >= 2) %>% 
  arrange(cluster_id,earliest_year,desc(n)) %>% 
  distinct(cluster_id,.keep_all = T) #per author, we take a destination they went first (some people have been at multiple destinations but their lesser destination is removed)

movers_per_region_count <- 
  destination_per_clusterid %>% 
  group_by(origin_region) %>% 
  tally() 

country_destinations_per_region_count <- 
  destination_per_clusterid %>% 
  group_by(origin_region, pub_country) %>% 
  tally() 

region_destinations_per_region_count <- 
  destination_per_clusterid %>% 
  group_by(origin_region, pub_country) %>% 
  tally() 
