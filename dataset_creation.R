# Dataset creation for blockmodel analysis
# September 16, 2022
# Tyler Barrett


# load packages
library(tidyverse)
library(igraph)

# set file path
fp <- "/Users/tylerbarrett/Library/CloudStorage/Box-Box/EEID_Data"


###################
## Naming Networks
##################

# read in the naming network (freetime, farming help, food help) edge data
name_table <- read_csv(paste0(fp, "/EEID_Data_public/network_edge_data/Name_Table.csv"))

# create multirelational edgelist from naming network data
edgelist <- name_table %>%
  select(social_netid, named_person_social_netid, network_question_number) %>%
  rename(relation = network_question_number, ego_id = social_netid,
         alter_id = named_person_social_netid) %>%
  mutate(value_of_tie = 1, .after = alter_id) %>%
  mutate(relation = recode(relation, `1` = "freetime", `2` = "farm_help_received",
                                  `3` = "farm_help_provided", `4` = "food_help_received",
                                  `5` = "food_help_provided"))
  #filter(grepl("A-SNH|D-SNH", alter_id)) # exclude edges where non-participant is named

# plot the freetime network
freetime_edgelist <- edgelist %>%
  filter(relation == "freetime") %>%
  select(ego_id, alter_id) %>%
  as.matrix()

freetime_net <- graph_from_edgelist(freetime_edgelist, directed = TRUE)

V(freetime_net)$label <- ""
plot(freetime_net, vertex.size = 5, edge.arrow.size = 0.5)

# plot the farm help received network
farm_help_received_edgelist <- edgelist %>%
  filter(relation == "farm_help_received") %>%
  select(ego_id, alter_id) %>%
  as.matrix()

farm_help_received_net <- graph_from_edgelist(farm_help_received_edgelist, directed = TRUE)

V(farm_help_received_net)$label <- ""
plot(farm_help_received_net, vertex.size = 5, edge.arrow.size = 0.5)

# plot the farm help provided network
farm_help_provided_edgelist <- edgelist %>%
  filter(relation == "farm_help_provided") %>%
  select(ego_id, alter_id) %>%
  as.matrix()

farm_help_provided_net <- graph_from_edgelist(farm_help_provided_edgelist, directed = TRUE)

V(farm_help_provided_net)$label <- ""
plot(farm_help_provided_net, vertex.size = 5, edge.arrow.size = 0.5)

# plot the food help received network
food_help_received_edgelist <- edgelist %>%
  filter(relation == "food_help_received") %>%
  select(ego_id, alter_id) %>%
  as.matrix()

food_help_received_net <- graph_from_edgelist(food_help_received_edgelist, directed = TRUE)

V(food_help_received_net)$label <- ""
plot(food_help_received_net, vertex.size = 5, edge.arrow.size = 0.5)

# plot the food help provided network
food_help_provided_edgelist <- edgelist %>%
  filter(relation == "food_help_provided") %>%
  select(ego_id, alter_id) %>%
  as.matrix()

food_help_provided_net <- graph_from_edgelist(food_help_provided_edgelist, directed = TRUE)

V(food_help_provided_net)$label <- ""
plot(food_help_provided_net, vertex.size = 5, edge.arrow.size = 0.5)

##############################
## Volume Intersection Network
##############################

# read in volume intersection edge data
volume_intersection <- read_csv(paste0(fp, "/EEID_Data_public/network_edge_data/Volume_Intersection.csv"))

# create volume intersection edgelist
volume_intersection <- volume_intersection %>%
  filter(!grepl(".Z", id1)) %>% # filter to only humans
  filter(!grepl(".Z", id2)) %>%
  slice_max(VI_95, prop = 0.05) %>% # identify the op 5% of overlap based on VI_95
  select(id1, id2, VI_95) %>%
  mutate(id1 = sub("A.SNH", "A-SNH", id1)) %>% # change "." to "-"
  mutate(id2 = sub("A.SNH", "A-SNH", id2)) %>%
  mutate(id1 = sub("D.SNH", "D-SNH", id1)) %>%
  mutate(id2 = sub("D.SNH", "D-SNH", id2)) %>%
  rename(ego_id = id1, alter_id = id2, value_of_tie = VI_95) %>%
  mutate(relation = "home_range")

# plot the network
volume_intersection_edgelist <- volume_intersection %>%
  select(ego_id, alter_id) %>%
  as.matrix()

volume_intersection_net <- graph_from_edgelist(volume_intersection_edgelist,
                                               directed = FALSE)

V(volume_intersection_net)$label <- ""
plot(volume_intersection_net, vertex.size = 5)

# bind with edgelist
edgelist <- bind_rows(edgelist, volume_intersection)


#######################
## Bipartite Relations
######################

# read in demographic survey data
demo_df <- read_csv(paste0(fp, "/EEID_Data_public/clean_data_tables/Survey_Demographic_Health.csv"))

# select relevant data
demo_df <- demo_df %>%
  select(social_netid, main_activity, other_activity_specifiy, cats, dogs, chickens,
         ducks, geese, turkeys, zebu, goats, pigs, sheep, rabbits, fish,
         grew_crops, crops_avocado, crops_bamboo, crops_banana,
         crops_beans, crops_breadfruit, crops_carrots, crops_cassava, crops_chilis,
         crops_cloves, crops_cloves, crops_cocoa, crops_coconut, crops_coffee,
         crops_cola, crops_cucumber, crops_eggplant, crops_ginger, crops_greens,
         crops_katy, crops_lemon_lime, crops_lychee, crops_maize, crops_mango,
         crops_nuts, crops_onions, crops_orange, crops_pineapple, crops_pulses,
         crops_rice, crops_sugar, crops_sweet_potato, crops_taro, crops_tobacco,
         crops_tomatoes, crops_vanilla) %>%
  mutate(social_netid = sub("A.SNH", "A-SNH", social_netid)) %>% # change "." to "-"
  mutate(social_netid = sub("D.SNH", "D-SNH", social_netid))


### create shared activities edgelist

# create incidence matrix
main_activity_df <- demo_df %>%
  select(social_netid, main_activity, other_activity_specifiy) %>%
  pivot_longer(!social_netid, names_to = "primary_or_secondary",
               values_to = "activity") %>%
  filter(!is.na(activity)) %>%
  mutate(value = 1) %>%
  select(social_netid, activity, value) %>%
  pivot_wider(id_cols = social_netid, names_from = activity,
              values_from = value) %>%
  replace(is.na(.), 0)

# convert dataframe to matrix
main_activity_mat <- main_activity_df %>%
  column_to_rownames("social_netid") %>%
  as.matrix()
  
# create bipartite network
main_activity_net <- graph_from_incidence_matrix(main_activity_mat)
  
# project bipartite network
main_activity_proj <- bipartite_projection(main_activity_net, multiplicity = TRUE)

# select projection 1
main_activity_personproj <- main_activity_proj[[1]]

# plot the projection
V(main_activity_personproj)$label = ""
plot(main_activity_personproj, vertex.size = 5,
     edge.width = E(main_activity_personproj)$weight)

# create edgelist with values
main_activity_edgelist <- as_long_data_frame(main_activity_personproj)

# select relevant variables and rename
main_activity_edgelist <- main_activity_edgelist %>%
  select(from_name, to_name, weight) %>%
  rename(ego_id = from_name, alter_id = to_name, value_of_tie = weight) %>%
  mutate(relation = "shared_activities")

# bind with main edgelist
edgelist <- bind_rows(edgelist, main_activity_edgelist)


### create common animals edgelist

# create incidence matrix
common_animals_df <- demo_df %>%
  select(social_netid, cats, dogs, chickens, ducks, geese, turkeys, zebu,
         goats, pigs, sheep, rabbits, fish) %>%
  mutate(cats = if_else(cats >= 1, 1, 0)) %>% # any non-zero amount of cats = 1
  mutate(dogs = if_else(dogs >= 1, 1, 0)) %>%
  mutate(chickens = if_else(chickens >= 1, 1, 0)) %>%
  mutate(ducks = if_else(ducks >= 1, 1, 0)) %>%
  mutate(geese = if_else(geese >= 1, 1, 0)) %>%
  mutate(turkeys = if_else(turkeys >= 1, 1, 0)) %>%
  mutate(zebu = if_else(zebu >= 1, 1, 0)) %>%
  mutate(goats = if_else(goats >= 1, 1, 0)) %>%
  mutate(pigs = if_else(pigs >= 1, 1, 0)) %>%
  mutate(sheep = if_else(sheep >= 1, 1, 0)) %>%
  mutate(rabbits = if_else(rabbits >= 1, 1, 0)) %>%
  mutate(fish = if_else(fish >= 1, 1, 0))

# convert dataframe to matrix
common_animals_mat <- common_animals_df %>%
  column_to_rownames("social_netid") %>%
  as.matrix()

# create bipartite network
common_animals_net <- graph_from_incidence_matrix(common_animals_mat)

# project bipartite network
common_animals_proj <- bipartite_projection(common_animals_net, multiplicity = TRUE)

# select projection 1
common_animals_personproj <- common_animals_proj[[1]]

# plot the projection without isolates
isolated_common_animals = which(degree(common_animals_personproj) == 0)
common_animals_personproj = delete.vertices(common_animals_personproj,
                                            isolated_common_animals)
V(common_animals_personproj)$label = ""
plot(common_animals_personproj, vertex.size = 5,
     edge.width = E(common_animals_personproj)$weight)

# create edgelist with values
common_animals_edgelist <- as_long_data_frame(common_animals_personproj)

# select relevant variables and rename
common_animals_edgelist <- common_animals_edgelist %>%
  select(from_name, to_name, weight) %>%
  rename(ego_id = from_name, alter_id = to_name, value_of_tie = weight) %>%
  mutate(relation = "common_animals")

# bind with main edgelist
edgelist <- bind_rows(edgelist, common_animals_edgelist)


### create common crops edgelist

# create incidence matrix

# small function to convert yes/no to 1/0
yes_no_function <- function(x) {
  if_else(x == "Yes", 1, 0)
}

common_crops_df <- demo_df %>%
  select(social_netid, crops_avocado, crops_bamboo, crops_banana,
         crops_beans, crops_breadfruit, crops_carrots, crops_cassava, crops_chilis,
         crops_cloves, crops_cloves, crops_cocoa, crops_coconut, crops_coffee,
         crops_cola, crops_cucumber, crops_eggplant, crops_ginger, crops_greens,
         crops_katy, crops_lemon_lime, crops_lychee, crops_maize, crops_mango,
         crops_nuts, crops_onions, crops_orange, crops_pineapple, crops_pulses,
         crops_rice, crops_sugar, crops_sweet_potato, crops_taro, crops_tobacco,
         crops_tomatoes, crops_vanilla) %>%
  mutate(across(!social_netid, yes_no_function)) %>%
  replace(is.na(.), 0)

# convert dataframe to matrix
common_crops_mat <- common_crops_df %>%
  column_to_rownames("social_netid") %>%
  as.matrix()

# create bipartite network
common_crops_net <- graph_from_incidence_matrix(common_crops_mat)

# project bipartite network
common_crops_proj <- bipartite_projection(common_crops_net, multiplicity = TRUE)

# select projection 1
common_crops_personproj <- common_crops_proj[[1]]

# plot the projection without isolates
isolated_common_crops = which(degree(common_crops_personproj) == 0)
common_crops_personproj = delete.vertices(common_crops_personproj,
                                            isolated_common_crops)
V(common_crops_personproj)$label = ""
plot(common_crops_personproj, vertex.size = 5,
     edge.width = E(common_crops_personproj)$weight)

# create edgelist with values
common_crops_edgelist <- as_long_data_frame(common_crops_personproj)

# select relevant variables and rename
common_crops_edgelist <- common_crops_edgelist %>%
  select(from_name, to_name, weight) %>%
  rename(ego_id = from_name, alter_id = to_name, value_of_tie = weight) %>%
  mutate(relation = "common_crops")

# bind with main edgelist
edgelist <- bind_rows(edgelist, common_crops_edgelist)

########################
## save edgelist as .csv
########################

write_csv(edgelist, file = "blockmodel_dataset.csv")
