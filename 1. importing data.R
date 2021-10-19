#Importing data

load("wos_data.RData")
pacman::p_load(dplyr, countrycode)
countries_to_regions <- read.csv("countries_to_regions.csv")

# making some small corrections to the dataset (universities in North Macedonia were missing the name of the country) #
publication_list_all <- publication_list_all %>% 
  mutate(pub_country = if_else(is.na(pub_country) & pub_org_name !="EURATOM", "North Macedonia", pub_country))

#########################################################################
#########################################################################
########## GETTING A DATASET OF ALL ELIGIBLE AUTHORS GLOBALLY ########## 
#########################################################################
#########################################################################

# making a dataset with publications only for authors that fulfill criteria:
# 1. only from one country
# 2. only from certain disciplines (those that are well covered in wos, and don't have abnormally high coauthorship rates)

####################################################################################
# STEP ONE: Find authors who are from a single country at order_of_publishing == 1 #
####################################################################################

length(unique(publication_list_all$cluster_id)) #at this point we have 525,892 researchers

step1 <- publication_list_all %>% #take the main dataset
  filter(order_of_publishing == 1) %>% #include only rows where it represents the earliest article for that author.
  group_by(cluster_id) %>% #since we now be left with some cluster_ids who have 2 article rows (because some have multiple affilations) i need to check that all affilations are from the same country
  mutate(number_of_distinct_countries = n_distinct(pub_country)) %>% #checking if the rows have the same country affilation
  filter(number_of_distinct_countries == 1, #ensuring that only cluster_ids with only 1 distinct country affilation are included
         !is.na(pub_org)) %>%  #we also can't have NA as their first affilation because we then don't know where they start. Therefore I remove them here.
  select(cluster_id) %>% #the next step is to merge all the rows for non "earliest-timepoint" articles. Therefore, i'm preparing to do a join to the "publication_list_all" dataframe, which should result in us being left with only the rows in "publication_list_all" that match with the cluster_ids we identified as being eligible 
  distinct(cluster_id) %>%  #only taking distinct ones (since for some cluster_ids we would have 2 rows if they had 2 eligible "earliest-timepoint" articles)
  left_join(publication_list_all, by = "cluster_id") %>%  #now we have a dataset that fulfills criteria 1
  ungroup()

length(unique(step1$cluster_id)) # we now have 501,491 researchers
length(unique(publication_list_all$cluster_id))-length(unique(step1$cluster_id)) # change = -24401

#####################################################
# STEP TWO: Keeping only well-covered disciplines #
#####################################################

# in order to decide on what disciplines should be included, I looked at the coverage
wos_covered_disciplines <- publication_info %>%
  distinct(ut, .keep_all = T) %>%
  group_by(discipline) %>%
  summarise(proportion_of_refs_covered = mean(n_refs_1980_covered/n_refs, na.rm =T)) %>%
  filter(proportion_of_refs_covered >= .6,#Biology, Biomedical Research, Chemistry, Clinical Medicine, Earth and Space, Engineering & tech, Health, Maths, Physics, & Psychology are all >60% covered
         !is.na(discipline)) 

step2 <- step1 %>% #taking the dataset made in step 2 above
  left_join(publication_info, by = c("ut")) %>% #joining it with the publication_info dataset, in order to get discipline information for each publication
  group_by(cluster_id) %>% 
  count(discipline) %>% # i then count how many articles have each disciples per author
  slice_max(n) %>% #then i make a dataframe showing the discipline per author with the highest number of articles
  filter(discipline %in% wos_covered_disciplines$discipline) %>% #i keep only authors where their most popular discipline is >=60% covered
  select(cluster_id, discipline) %>% #selecting the remaining authors (and also their discipline)
  left_join(step1, by = "cluster_id") %>%  #joining our selection of authors from only our chosen disciplines, with the information present in the previous dataframe.
  ungroup()

length(unique(step2$cluster_id)) #at this point we have 480,964 researchers
length(unique(step1$cluster_id))-length(unique(step2$cluster_id)) #change = -20527

################################################################################
# STEP THREE: removing any subdisciplines that have too high coauthorship rates #
################################################################################
#here i look at mean numbers of co-authors per specialty
average_number_of_coauthors_per_specialty <- publication_info %>% 
  distinct(ut, .keep_all = T) %>%
  group_by(specialty) %>% 
  summarise(mean_n_authors = mean(n_authors, na.rm=T),
            median_n_authors = median(n_authors, na.rm=T)) %>% 
  arrange(desc(mean_n_authors)) #Nuclear & Particle Physics has 139 mean authors compared to next nearest 18.8, so anyone working primarily in this specialty will be removed

#here I add the each researchers main specialty to the dataset, and then remove researchers who focus on the specialty "Nuclear & Particle Physics"
step3 <- step2 %>% #take the step 3 dataset (i.e. only wos covered disciplines)
  select(-discipline) %>% #...remove this column since it will be duplicate when we...
  left_join(publication_info, by = c("ut")) %>% #...join the author information to the publication metadata
  group_by(cluster_id) %>% 
  add_count(specialty, name = "n_specialty_articles") %>% #per cluster id, this provides a count of number of articles with this specialty...
  add_count(discipline, name = "n_discipline_articles") %>% #and also discipline
  distinct(cluster_id, discipline, specialty, .keep_all = T) %>% #keeping one row per cluster_id
  select(cluster_id, discipline, n_discipline_articles,specialty, n_specialty_articles) %>% 
  filter(!is.na(discipline),
         !is.na(specialty)) %>% #keeping only individuals with a main discipline and specialty
  arrange(cluster_id, desc(n_discipline_articles), desc(n_specialty_articles)) %>% #this arranges the dataset so that an individuals top discipline is at the top, which is further ordered by specialty 
  slice(1) %>% #taking an individuals top specialty....
  select(cluster_id, specialty) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  filter(specialty != "Nuclear & Particle Physics") %>% #...and excluding  cluster ids with a specialty of nuclear physics.
  left_join(step2, by = "cluster_id") #then joining the whole dataset back got all individuals that weren't excluded.

length(unique(step3$cluster_id)) #at this point we have 474,930 researchers
length(unique(step2$cluster_id))-length(unique(step3$cluster_id)) #change = -6034

###################################################################
# STEP FOUR: MAKING THE FINAL DATASET OF ALL ELIGIBLE RESEARCHERS #
###################################################################

# Here I chose a institute which represents the "origin" of the researcher. 
step4 <- 
  step3 %>% 
  filter(order_of_publishing == 1) %>% #take the data of everyone at order_of_publishing = 1...
  select(cluster_id, pub_org_name) %>% 
  left_join(publication_list_all, by = c("cluster_id", "pub_org_name")) %>% #... get their publications
  group_by(cluster_id, pub_org_name) %>% 
  mutate(number_of_publications_with_this_affilation = n()) %>% #for each affilation measure how many times researchers published with this affilation during career.
  distinct(cluster_id, pub_org_name, number_of_publications_with_this_affilation, .keep_all = T) %>% 
  select(cluster_id, pub_org_name, number_of_publications_with_this_affilation, lr_univ_id,pub_country) %>% 
  group_by(cluster_id) %>% 
  arrange(cluster_id, desc(number_of_publications_with_this_affilation),lr_univ_id) %>% #important ordering to ensure the comment below is correct.
  mutate(origin_institution = first(pub_org_name), #this makes a variable of the origin institution. If there were multiple institutions at order_of_publishing == 1, then this takes the institution where the researcher had the most publications in his/her career. if it is a tie, then the leiden ranked university is chosen. If it is still a tie then it is selected alphabetically.
         origin_country = first(pub_country), #new variable: what is the origin country of the researcher
         origin_leiden_ranked = first(if_else(is.na(lr_univ_id), 0, 1))) %>% #new variable: is the origin institute Leiden ranked?
  select(cluster_id, origin_institution, origin_country,origin_leiden_ranked) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  left_join(step3, by ="cluster_id") #creates a dataset with information about the authors, including origin info, + each UT (but with no further meta data)

global_mobility_eligible_researchers <- step4 %>% #this becomes the dataset that contains descriptive information about all of our potential matches
  filter(origin_institution == pub_org_name) %>% 
  arrange(cluster_id, order_of_publishing) %>% 
  group_by(cluster_id) %>%
  mutate(final_article_at_origininstitution_year = last(career_year)) %>% 
  distinct(cluster_id, .keep_all = T) %>% 
  select(cluster_id, final_article_at_origininstitution_year) %>% 
  ungroup()%>% 
  left_join(step4, by = "cluster_id") %>% 
  left_join(publication_info %>% select(ut, n_authors, n_countries), by = "ut") %>% #adding number of authors on paper, and number of countries
  mutate(n_coauthors = n_authors - 1) %>% 
  left_join(countries_to_regions, by = "origin_country") %>%  #adding in region
  rename(origin_region = region)
  

length(unique(global_mobility_eligible_researchers$cluster_id)) #at this point we have 474,930 researchers
length(unique(publication_list_all$cluster_id))-length(unique(global_mobility_eligible_researchers$cluster_id)) #total exclusion to "enrollment" = 50,962

save(global_mobility_eligible_researchers, file = "global_mobility_eligible_researchers.RData")
