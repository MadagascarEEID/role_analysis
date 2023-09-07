# Role analysis script
# Created: September 07, 2023
# Tyler Barrett

# load packages
library(tidyverse)
library(ideanet)

# set file path
fp <- "/Users/tylerbarrett/Library/CloudStorage/Box-Box/EEID_Data"

###################################
## Create nodelist for each village
###################################

# read in demographic data
demo_df <- read_csv(paste0(fp, "/EEID_Data_public/clean_data_tables/Survey_Demographic_Health.csv"))

# select relevant variables - just basic demographics for now
demo_df <- demo_df %>%
  select(social_netid, village, gender, age, school_level, main_activity)

# separate nodelists for each village
nl_mandena <- demo_df %>%
  filter(village == "Mandena")
nl_sarahandrano <- demo_df %>%
  filter(village == "Sarahandrano")
nl_andatsakala <- demo_df %>%
  filter(village == "Andatsakala" | village == "Ampandrana")

##################################
## Create multirelational edgelist
##################################

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
                           `5` = "food_help_provided")) %>%
  filter(!grepl("uid", alter_id)) %>% # exclude edges where non-participant is named
  filter(ego_id != alter_id) # remove self loops

# create separate edgelists for each village
el_mandena <- edgelist %>%
  filter(grepl("A.SNH", ego_id))
el_sarahandrano <- edgelist %>%
  filter(grepl("D.SNH", ego_id))
el_andatsakala <- edgelist %>%
  filter(grepl("E.SNH", ego_id))

##################################
## Reformat data for role analysis
##################################

# create a list object containing nodelist and edgelist for each village
net_data <- list(mandena = list(nl = nl_mandena,
                                el = el_mandena),
                 sarahandrano = list(nl = nl_sarahandrano,
                                     el = el_sarahandrano),
                 andatsakala = list(nl = nl_andatsakala,
                                    el = el_andatsakala))
#saveRDS(net_data, "net_data.rds")

# run for loop to iterate netwrite function over data for each village
netwrite_list <- list()
for (i in names(net_data)) {
  village <- net_data[[i]]
  
  # execute netwrite function
  
  netwrite(nodelist = village$nl,
           node_id = "social_netid",
           i_elements = village$el$ego_id,
           j_elements = village$el$alter_id,
           type = village$el$relation,
           directed = TRUE)
  
  # add netwrite output to a list
  
  netwrite_list[[i]] <- list(
    bicomponent_list = bicomponent_list,
    edgelist = edgelist,
    edgelist_list = edgelist_list,
    largest_bi_component = largest_bi_component,
    largest_component = largest_component,
    largest_component_list = largest_component_list,
    network = network,
    network_list = network_list,
    node_measure_plot = node_measure_plot,
    node_measure_plot_list = node_measure_plot_list,
    node_measures = node_measures,
    node_measures_list = node_measures_list,
    system_level_measures = system_level_measures,
    system_level_measures_list = system_level_measures_list,
    system_measure_plot = system_measure_plot,
    system_measure_plot_list = system_measure_plot_list,
    this_igraph = this_igraph
  )
}

################
## Role analysis
################

# run for loop to iterate role analysis function over netwrite object for each village

### checking w/ Tom on why for loop is producing error, works fine without the loop

role_analysis_list <- vector("list", length(netwrite_list))
for (i in names(netwrite_list)) {
  village <- netwrite_list[[i]]
  
  # execute role analysis function
  
  role_analysis(graph = village$network_list,
                nodes = village$node_measures,
                directed = TRUE,
                method = "cluster",
                min_partitions = 2,
                max_partitions = 10,
                viz = TRUE,
                retain_variables = TRUE,
                cluster_summaries = TRUE)
  
  # add role analysis output to a list
  
  role_analysis_list[[i]] <- list(
    cluster_assignments = cluster_assignments,
    cluster_dendrogram = cluster_dendrogram,
    cluster_modularity = cluster_modularity,
    cluster_relations_heatmaps = cluster_relations_heatmaps,
    cluster_relations_sociogram = cluster_relations_sociogram,
    cluster_sociogram = cluster_sociogram,
    cluster_summaries = cluster_summaries,
    cluster_summaries_cent = cluster_summaries_cent,
    cluster_summaries_correlations = cluster_summaries_correlations,
    cluster_summaries_triad = cluster_summaries_triad,
    clustering_variables = clustering_variables
  )
}



role_analysis(graph = network_list,
              nodes = node_measures,
              directed = TRUE,
              method = "cluster",
              min_partitions = 2,
              max_partitions = 10,
              viz = TRUE,
              retain_variables = TRUE,
              cluster_summaries = TRUE)


