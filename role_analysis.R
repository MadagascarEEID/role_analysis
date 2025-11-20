# Companion Code to the Following Manuscript:
  # "Identifying Social-Epidemiological Roles Associated with
  # Viral Exposure Using Regular Equivalence Blockmodeling"

# Created by Tyler Barrett on September 07, 2023

# This R script contains the code to prepare the social network data
# for analysis, conduct the role analysis using regular equivalence
# blockmodeling, and model the relationship between roles and viral
# exposure.

# Data Requirements
  # This script requires the following data files (not included):
  # 1. Survey_Demographic_Health.csv
  # 2. Name_Table.csv
  # 3. C5_All_HitsList.csv and C10_All_HitsList.csv (VirScan Results)

# The data are available upon request from Tyler Barrett (tyler.barrett@duke.edu)
# and are not publicly available due to privacy or ethical considerations.

#################
#   Preamble    #
#################

# Load Packages
  library(tidyverse)
  library(gtsummary)
  library(igraph)
  library(ggraph)
  library(ggridges)
  library(patchwork)
  library(ideanet)
  library(rethinking)
  library(glmmTMB)
  library(MuMIn)

# set File Path
  if (paste(Sys.info()[[1]], collapse=" ") == "Windows"){
  fp <- "C:/Users/tmbar/Box"
}else{
  fp <- "/Users/tylerbarrett/Library/CloudStorage/Box-Box"
}

#################
#   FUNCTIONS   #
#################

# Construct Networks for Each Relation
  construct_networks <- function(el, relations, nl) {
    networks <- list()
    
    for (i in relations) {
      
      # Filter Edgelist to Specified Relation
        filtered_el <- el %>% filter(relation == i)
      
      # Create Network
        network <- graph_from_data_frame(filtered_el, nl, directed = TRUE)
      
      # Store Network in List
        networks[[i]] <- network
    }
    
    return(networks)
  }
  
# Calculate Centrality Measures for Each Village's Network
  calculate_centrality <- function(network) {
    # Create distance weights for betweenness and closeness (invert the strength weights)
    # Using 1/weight transformation since weights range from 1-5
      E(network)$weight_distance <- 1 / E(network)$weight
    
    data.frame(
      social_netid = V(network)$name,
      weighted_degree = strength(network, mode = "total", weights = E(network)$weight),  # Weighted degree
      betweenness = betweenness(network, 
                                directed = TRUE, 
                                weights = E(network)$weight_distance),  # Uses inverted weights
      eigenvector = eigen_centrality(network, 
                                     directed = TRUE, 
                                     weights = E(network)$weight)$vector,  # Uses original weights
      closeness = closeness(network, 
                            mode = "total", 
                            weights = E(network)$weight_distance)  # Uses inverted weights
    )
  }
  
# Compute Network Density and Summarize Centrality Measures
  network_summary_metrics <- function(network) {
    
    # Get Node and Edge Counts
      nodes <- gorder(network)
      edges <- gsize(network)
    
    # Compute Density
      density <- igraph::edge_density(network)
      
    # Compute Centrality Measures
      indegree <- degree(network, mode = "in")
      outdegree <- degree(network, mode = "out")
      total_degree <- degree(network)
      betweenness <- betweenness(network)
      power <- power_centrality(network, exponent = 0.1)
      eigenvector <- eigen_centrality(network)$vector
      closeness <- closeness(network, mode = "total")
    
    # Store Centrality Measures in a Dataframe
      centrality_df <- data.frame(
        indegree = indegree,
        outdegree = outdegree,
        total_degree = total_degree,
        betweenness = betweenness,
        power = power,
        eigenvector = eigenvector,
        closeness = closeness
      )
    
    # Compute Means and Standard Deviations
      centrality_summary <- centrality_df %>%
        summarise(
          mean_indegree = mean(indegree),
          sd_indegree = sd(indegree),
          mean_outdegree = mean(outdegree),
          sd_outdegree = sd(outdegree),
          mean_total_degree = mean(total_degree),
          sd_total_degree = sd(total_degree),
          mean_betweenness = mean(betweenness),
          sd_betweenness = sd(betweenness),
          mean_power = mean(power),
          sd_power = sd(power),
          mean_eigenvector = mean(eigenvector),
          sd_eigenvector = sd(eigenvector),
          mean_closeness = mean(closeness, na.rm = TRUE),
          sd_closeness = sd(closeness, na.rm = TRUE),
          density = density,
          nodes = nodes,
          edges = edges
        )
      
    return(centrality_summary)
  }

# Triad and Centrality Summary Plots
  plot_role_summaries <- function(clustering_variables, cluster_groups) {
    
    # Prepare data for triad positions
      triad_df <- clustering_variables %>%
        select(id, cluster, any_of(c("freetime_021C_B", "freetime_021C_S", "freetime_021D_E", "freetime_021D_S", 
                             "freetime_021U_E", "freetime_021U_S", "freetime_030T_S", "freetime_111D_S", 
                             "freetime_021C_E", "freetime_111U_E", "freetime_030T_E", "freetime_111D_B", 
                             "freetime_111D_E", "freetime_111U_S", "freetime_201_S", "freetime_111U_B", 
                             "freetime_201_B", "freetime_120C_S", "freetime_120C_B", "freetime_120C_E", 
                             "freetime_030C", "freetime_030T_B", "freetime_210_B", "freetime_300", 
                             "freetime_210_S", "freetime_210_E", "freetime_120D_S", "freetime_120U_S", 
                             "freetime_120D_E", "freetime_120U_E", "food_help_received_021C_B", 
                             "food_help_received_021C_E", "food_help_received_021C_S", "food_help_received_021D_E", 
                             "food_help_received_021D_S", "food_help_received_021U_E", "food_help_received_021U_S", 
                             "food_help_received_111D_S", "food_help_received_111U_E", "food_help_received_111D_B", 
                             "food_help_received_111D_E", "food_help_received_111U_S", "food_help_received_030T_S", 
                             "food_help_received_120D_E", "food_help_received_201_B", "food_help_received_201_S", 
                             "food_help_received_030T_B", "food_help_received_030T_E", "food_help_received_120U_E", 
                             "food_help_received_111U_B", "food_help_received_120D_S", "food_help_received_120C_B", 
                             "food_help_received_120U_S", "food_help_received_120C_S", "food_help_received_120C_E", 
                             "food_help_received_300", "food_help_provided_021D_E", "food_help_provided_021U_E", 
                             "food_help_provided_021U_S", "food_help_provided_021C_S", "food_help_provided_021D_S", 
                             "food_help_provided_111D_S", "food_help_provided_021C_B", "food_help_provided_021C_E", 
                             "food_help_provided_111D_B", "food_help_provided_111U_E", "food_help_provided_111D_E", 
                             "food_help_provided_120D_S", "food_help_provided_111U_S", "food_help_provided_030T_S", 
                             "food_help_provided_111U_B", "food_help_provided_210_S", "food_help_provided_210_E", 
                             "food_help_provided_210_B", "food_help_provided_120U_S", "food_help_provided_120C_B", 
                             "food_help_provided_030T_B", "food_help_provided_120D_E", "food_help_provided_201_S", 
                             "food_help_provided_201_B", "food_help_provided_030T_E", "food_help_provided_030C", 
                             "food_help_provided_120C_E", "food_help_provided_120C_S", "food_help_provided_120U_E", 
                             "farm_help_received_021C_B", "farm_help_received_021C_E", "farm_help_received_021C_S", 
                             "farm_help_received_021U_S", "farm_help_received_111D_B", "farm_help_received_111U_B", 
                             "farm_help_received_111U_S", "farm_help_received_111D_E", "farm_help_received_120U_S", 
                             "farm_help_received_021U_E", "farm_help_received_111D_S", "farm_help_received_021D_E", 
                             "farm_help_received_021D_S", "farm_help_received_111U_E", "farm_help_received_030T_E", 
                             "farm_help_received_030T_S", "farm_help_received_120C_B", "farm_help_received_120D_E", 
                             "farm_help_received_201_S", "farm_help_received_210_S", "farm_help_received_120C_E", 
                             "farm_help_received_201_B", "farm_help_received_210_B", "farm_help_received_210_E", 
                             "farm_help_received_300", "farm_help_received_120C_S", "farm_help_received_120D_S", 
                             "farm_help_received_120U_E", "farm_help_received_030T_B", "farm_help_provided_021D_E", 
                             "farm_help_provided_021C_S", "farm_help_provided_021D_S", "farm_help_provided_021U_S", 
                             "farm_help_provided_021C_B", "farm_help_provided_021C_E", "farm_help_provided_021U_E", 
                             "farm_help_provided_111D_B", "farm_help_provided_111U_B", "farm_help_provided_111U_E", 
                             "farm_help_provided_111U_S", "farm_help_provided_030T_S", "farm_help_provided_111D_S", 
                             "farm_help_provided_120D_S", "farm_help_provided_030C", "farm_help_provided_030T_E", 
                             "farm_help_provided_120U_E", "farm_help_provided_111D_E", "farm_help_provided_120C_S", 
                             "farm_help_provided_120C_E", "farm_help_provided_120C_B", "farm_help_provided_120D_E", 
                             "farm_help_provided_120U_S", "farm_help_provided_300", "farm_help_provided_210_B", 
                             "farm_help_provided_030T_B", "farm_help_provided_201_B", "farm_help_provided_210_S")))
        triad_summary_df <- triad_df %>% 
          mutate(group = factor(case_when(
            cluster %in% cluster_groups$most_popular ~ "Most Popular",
            cluster %in% cluster_groups$core ~ "Core",
            cluster %in% cluster_groups$periphery ~ "Periphery"
          ), levels = c("Most Popular", "Core", "Periphery"))) %>% 
          group_by(group) %>% 
          summarise(across(matches("^(freetime|food_help_provided|food_help_received|farm_help_provided|farm_help_received)"), mean, na.rm = TRUE)) %>% 
          ungroup() %>% 
          mutate(across(where(is.numeric), ~ scale(.)[,1], .names = "std_{.col}")) %>% 
          select(group, starts_with("std_")) %>% 
          pivot_longer(cols = -group, names_to = "variable", values_to = "value") %>% 
          mutate(relation = case_when(
            str_detect(variable, "^std_freetime") ~ "Free Time",
            str_detect(variable, "^std_food_help_provided") ~ "Food Help Provided",
            str_detect(variable, "^std_food_help_received") ~ "Food Help Received",
            str_detect(variable, "^std_farm_help_provided") ~ "Farm Help Provided",
            str_detect(variable, "^std_farm_help_received") ~ "Farm Help Received"
          ),
          triad_position = str_remove(variable, "^(std_freetime_|std_food_help_provided_|std_food_help_received_|std_farm_help_provided_|std_farm_help_received_)")
          )

        # Make Plot
          triad_plot <- ggplot(triad_summary_df, aes(x = triad_position, y = value, fill = value > 0)) +
            geom_col(position = "dodge") +
            scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "maroon")) +
            xlab("Triad Position") +
            ylab("Mean Frequency (Standardized)") +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90, hjust = 1, color = "black", size = 14, face = "bold"),
                  axis.text.y = element_text(size = 14, face = "bold"),
                  axis.title.x = element_text(size = 16, face = "bold"),
                  axis.title.y = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 14, face = "bold")) +
            facet_grid(relation ~ group, scales = "free_y") +  # Separate plots for each category and facet by group
            theme(panel.border = element_rect(fill = NA, color = "black"))
          
        # Prepare data for centrality measures
          cent_df <- clustering_variables %>%
            select(id, cluster, any_of(c("freetime_total_degree", "freetime_in_degree", "freetime_out_degree", 
                                 "food_help_received_total_degree", "food_help_received_in_degree", "food_help_received_out_degree", 
                                 "food_help_provided_total_degree", "food_help_provided_in_degree", "food_help_provided_out_degree", 
                                 "farm_help_received_total_degree", "farm_help_received_in_degree", "farm_help_received_out_degree", 
                                 "farm_help_provided_total_degree", "farm_help_provided_in_degree", "farm_help_provided_out_degree", 
                                 "betweenness_scores", "freetime_betweenness_scores", "food_help_received_betweenness_scores", 
                                 "food_help_provided_betweenness_scores", "farm_help_received_betweenness_scores", "farm_help_provided_betweenness_scores", 
                                 "bonpow", "bonpow_negative", "freetime_bonpow", "freetime_bonpow_negative", 
                                 "food_help_received_bonpow", "food_help_received_bonpow_negative", "food_help_provided_bonpow", "food_help_provided_bonpow_negative", 
                                 "farm_help_received_bonpow", "farm_help_received_bonpow_negative", "farm_help_provided_bonpow", "farm_help_provided_bonpow_negative", 
                                 "eigen_centrality", "freetime_eigen_centrality", "food_help_received_eigen_centrality", 
                                 "food_help_provided_eigen_centrality", "farm_help_received_eigen_centrality", "farm_help_provided_eigen_centrality", 
                                 "closeness_in", "closeness_out", "closeness_undirected", 
                                 "freetime_closeness_in", "freetime_closeness_out", "freetime_closeness_undirected", 
                                 "food_help_received_closeness_in", "food_help_received_closeness_out", "food_help_received_closeness_undirected", 
                                 "food_help_provided_closeness_in", "food_help_provided_closeness_out", "food_help_provided_closeness_undirected", 
                                 "farm_help_received_closeness_in", "farm_help_received_closeness_out", "farm_help_received_closeness_undirected", 
                                 "farm_help_provided_closeness_in", "farm_help_provided_closeness_out", "farm_help_provided_closeness_undirected")))

            cent_summary_df <- cent_df %>% 
              mutate(group = factor(case_when(
                cluster %in% cluster_groups$most_popular ~ "Most Popular",
                cluster %in% cluster_groups$core ~ "Core",
                cluster %in% cluster_groups$periphery ~ "Periphery"
              ), levels = c("Most Popular", "Core", "Periphery"))) %>% 
              group_by(group) %>% 
              summarise(across(matches("^(freetime|food_help_provided|food_help_received|farm_help_provided|farm_help_received)_"), mean, na.rm = TRUE)) %>% 
              ungroup() %>% 
              mutate(across(where(is.numeric), ~ scale(.)[,1], .names = "std_{.col}")) %>% 
              select(group, starts_with("std_")) %>% 
              pivot_longer(cols = -group, names_to = "variable", values_to = "value") %>% 
              mutate(relation = case_when(
                str_detect(variable, "^std_freetime") ~ "Free Time",
                str_detect(variable, "^std_food_help_provided") ~ "Food Help Provided",
                str_detect(variable, "^std_food_help_received") ~ "Food Help Received",
                str_detect(variable, "^std_farm_help_provided") ~ "Farm Help Provided",
                str_detect(variable, "^std_farm_help_received") ~ "Farm Help Received"
              ),
              centrality_measure = str_remove(variable, "^(std_freetime_|std_food_help_provided_|std_food_help_received_|std_farm_help_provided_|std_farm_help_received_)") %>%
                str_replace_all("_", " ") %>% 
                str_replace("betweenness scores", "Betweenness") %>%
                str_to_title()
              )

            # Make Plot
              cent_plot <- ggplot(cent_summary_df, aes(x = centrality_measure, y = value, fill = value > 0)) +
                geom_col(position = "dodge") +
                scale_fill_manual(values = c("TRUE" = "navy", "FALSE" = "maroon")) +
                xlab("Centrality Measure") +
                ylab("Mean Centrality Score (Standardized)") +
                theme_classic() +
                theme(legend.position = "none",
                      axis.text.x = element_text(angle = 40, hjust = 1, color = "black", size = 14, face = "bold"),
                      axis.text.y = element_text(size = 14, face = "bold"),
                      axis.title.x = element_text(size = 16, face = "bold"),
                      axis.title.y = element_text(size = 16, face = "bold"),
                      strip.text = element_text(size = 14, face = "bold")) +
                facet_grid(relation ~ group, scales = "free_y") +  # Groups on top, relations on the right
                theme(panel.border = element_rect(fill = NA, color = "black"))

            return(list(triad_plot = triad_plot, centrality_plot = cent_plot))
  }

# Extract Summary Edgelist from Role Analysis Results
  summary_el_extracter <- function(village_name) {
  
  # extract cluster assignments
  net_data[[village_name]]$nl <- net_data[[village_name]]$nl %>%
    mutate(cluster_assignment = as.factor(role_analysis_list[[village_name]]$cluster_assignments$best_fit))
  
  # construct edgelist
  el <- net_data[[village_name]]$el
  el$ego_cluster <- net_data[[village_name]]$nl$cluster_assignment[match(el$ego_id, net_data[[village_name]]$nl$social_netid)]
  el$alter_cluster <- net_data[[village_name]]$nl$cluster_assignment[match(el$alter_id, net_data[[village_name]]$nl$social_netid)]
  el <- el %>%
    select(ego_cluster, alter_cluster, relation)
  
  # count volume of ties for each dyad and relation
  el <- el %>%
    group_by(ego_cluster, alter_cluster, relation) %>%
    summarise(count = n(), .groups = 'drop')
  
  # make a table with the number of nodes per cluster
  cluster_n <- (table(net_data[[village_name]]$nl$cluster_assignment))
  
  return(list(cluster_n = cluster_n, el = el))
}

###############################
#   BRING IN NODE-LEVEL DATA  #
###############################

# Read in Demographic Data
  demo_df <- read_csv(paste0(fp, "/EEID_Data_public/clean_data_tables/NIH Human Surveys/Survey_Demographic_Health.csv"))
  
# Create Household Style of Life Index
  # Wall Construction
    demo_df <- demo_df %>%
      mutate(material_wall_index = 0) %>%
      mutate(material_wall_index = if_else(material_wall == "bamboo", 0, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "rafia", 0, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "ravenala", 0, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "mud", 0, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "compacted_earth", 0, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "wood_planks", 1, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "brick_unfired", 2, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "brick_fired", 2, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "metal_sheets", 3, material_wall_index)) %>%
      mutate(material_wall_index = if_else(material_wall == "cement", 4, material_wall_index)) %>%
      mutate(material_wall_index = scale(material_wall_index))
    
  # Roof Construction
    demo_df <- demo_df %>%
      mutate(material_roof_index = 0) %>%
      mutate(material_roof_index = if_else(material_roof == "bamboo", 0, material_roof_index)) %>%
      mutate(material_roof_index = if_else(material_roof == "thatch", 0, material_roof_index)) %>%
      mutate(material_roof_index = if_else(material_roof == "metal_sheets", 1, material_roof_index)) %>%
      mutate(material_roof_index = if_else(material_roof == "cement", 2, material_roof_index)) %>%
      mutate(material_roof_index = scale(material_roof_index))
    
  # Floor Construction
    demo_df <- demo_df %>%
      mutate(material_floor_index = 0) %>%
      mutate(material_floor_index = if_else(material_floor == "dirt", 0, material_floor_index)) %>%
      mutate(material_floor_index = if_else(material_floor == "bamboo", 0, material_floor_index)) %>%
      mutate(material_floor_index = if_else(material_floor == "rafia", 0, material_floor_index)) %>%
      mutate(material_floor_index = if_else(material_floor == "ravinala", 0, material_floor_index)) %>%
      mutate(material_floor_index = if_else(material_floor == "wood_planks", 1, material_floor_index)) %>%
      mutate(material_floor_index = if_else(material_floor == "cement", 2, material_floor_index)) %>%
      mutate(material_floor_index = scale(material_floor_index))
    
  # Sum Scores to Create Index
    demo_df <- demo_df %>%
      mutate(house_sol = material_wall_index + material_roof_index + material_floor_index) %>%
      mutate(house_sol = as.numeric(house_sol))
  
# Create Durable Goods Owned Index
    demo_df <- demo_df %>%
      mutate(own_cellphone = if_else(own_cellphone == "Yes", 1, 0)) %>%
      mutate(own_tv = if_else(own_tv == "Yes", 1, 0)) %>%
      mutate(own_bicycle = if_else(own_bicycle == "Yes", 1, 0)) %>%
      mutate(own_refrigerator = if_else(own_refrigerator == "Yes", 1, 0)) %>%
      mutate(own_motorcycle = if_else(own_motorcycle == "Yes", 1, 0)) %>%
      mutate(own_computer = if_else(own_computer == "Yes", 1, 0)) %>%
      mutate(own_generator = if_else(own_generator == "Yes", 1, 0))
  
    demo_df <- demo_df %>%
      mutate(goods_owned = own_cellphone + own_tv +own_bicycle + own_refrigerator +
               own_motorcycle + own_computer + own_generator)
  
    demo_df$goods_owned <- as.numeric(demo_df$goods_owned)
    hist(demo_df$goods_owned)
  
# Select Relevant Variables
  demo_df <- demo_df %>%
    select(social_netid, village, gender, age, school_level, main_activity,
           house_sol, goods_owned, household_size)

# Recode Ampandrana as Andatsakala
  demo_df <- demo_df %>%
    mutate(village = case_when(
      village == "Ampandrana" ~ "Andatsakala",
      TRUE ~ village
    ))

#######################################
#   CREATE MULTIRELATIONAL EDGELIST   #
#######################################

# Read in Naming Network (Freetime, Farming Help, Food Help) Edge Data
  name_table <- read_csv(paste0(fp, "/EEID_Data_public/network_edge_data/Name_Table.csv"))

# Create Multirelational Edgelist from Naming Network Data
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

# Create Separate Edgelists for Each Village
  el_mandena <- edgelist %>%
    filter(grepl("A.SNH", ego_id))
  el_sarahandrano <- edgelist %>%
    filter(grepl("D.SNH", ego_id))
  el_andatsakala <- edgelist %>%
    filter(grepl("E.SNH", ego_id))
  
# ISOLATES NAMED OTHER PEOPLE BUT NOT OTHER PEOPLE WHO PARTICIPATED IN THE SURVEY
  isolates_check <- demo_df %>%
    filter(social_netid %in% edgelist$ego_id | social_netid %in% edgelist$alter_id)
  setdiff(demo_df$social_netid, isolates_check$social_netid)
  
# Separate Node Lists for Each Village
  nl_mandena <- demo_df %>%
    filter(village == "Mandena")
  nl_sarahandrano <- demo_df %>%
    filter(village == "Sarahandrano")
  nl_andatsakala <- demo_df %>%
    filter(village == "Andatsakala")
  
##############################################
#   CHARACTERIZE NETWORKS AND PARTICIPANTS   #
##############################################

# Recode Occupation as Farmer vs. Non-Farmer
  demo_df <- demo_df %>%
    mutate(main_activity = case_when(
      str_detect(main_activity, "farm") ~ "farmer",
      is.na(main_activity) ~ NA_character_,
      TRUE ~ "non-farmer"))
  
# Make Table 1 
  tbl1 <- demo_df %>%
    tbl_summary(include = c(age, gender, school_level, main_activity,
                            house_sol, goods_owned, household_size),
                by = village,
                missing = "ifany")
  tbl2 <- demo_df %>%
    tbl_summary(include = c(age, gender, school_level, main_activity,
                            house_sol, goods_owned, household_size),
                missing = "ifany")
  # merge tables
    tbl_merge(tbls = list(tbl1, tbl2),
            tab_spanner = c("**Village**", "**All Villages**"))
    
# Construct All Five Networks for Each Village
  relations <- c("freetime", "food_help_received", "food_help_provided", "farm_help_received", "farm_help_provided")
  networks_mandena <- construct_networks(el_mandena, relations, nl_mandena)
  networks_sarahandrano <- construct_networks(el_sarahandrano, relations, nl_sarahandrano)
  networks_andatsakala <- construct_networks(el_andatsakala, relations, nl_andatsakala)
  
# Compute Density and Mean Std. Centrality
  villages <- c("mandena", "sarahandrano", "andatsakala")
  net_summaries <- data.frame()
  for (village in villages) {
    for (relation in relations) {
      
      # Get Network
        network <- get(paste0("networks_", village))[[relation]]
      
      # Compute Summary Stats
        summary_stats <- network_summary_metrics(network)
      
      # Add Village and Relation
        summary_stats$village <- village
        summary_stats$relation <- relation
      
      # Add to Dataframe
        net_summaries <- bind_rows(net_summaries, summary_stats)
    }
  }
  
# Save Table as .csv (Supplemental Table 1)
 #  write.csv(net_summaries, "net_summaries.csv")
    
# Create a Weighted Edgelist for Visualizing the Networks
  el_weighted_mandena <- el_mandena %>%
    group_by(ego_id, alter_id) %>%
    summarise(weight = n_distinct(relation), .groups = "drop")
  el_weighted_sarahandrano <- el_sarahandrano %>%
    group_by(ego_id, alter_id) %>%
    summarise(weight = n_distinct(relation), .groups = "drop")
  el_weighted_andatsakala <- el_andatsakala %>%
    group_by(ego_id, alter_id) %>%
    summarise(weight = n_distinct(relation), .groups = "drop")
  
# Construct Networks
  mandena_net <- graph_from_data_frame(el_weighted_mandena, nl_mandena, directed = TRUE)
  sarahandrano_net <- graph_from_data_frame(el_weighted_sarahandrano, nl_sarahandrano, directed = TRUE)
  andatsakala_net <- graph_from_data_frame(el_weighted_andatsakala, nl_andatsakala, directed = TRUE)
  
# Plot Networks
  # Define edge thickness scale
    edge_thickness_range <- c(0.5, 2)
    
  # Compute Strength Centrality
    V(mandena_net)$strength <- strength(mandena_net)
    V(sarahandrano_net)$strength <- strength(sarahandrano_net)
    V(andatsakala_net)$strength <- strength(andatsakala_net)
    
  # Define Edge Weights
    unique_weights <- sort(unique(E(mandena_net)$weight))
  
  # Create Plots
    plot_mandena <- ggraph(mandena_net, layout = "stress") +  
      geom_edge_link(aes(color = weight), 
                     alpha = 1,  
                     arrow = arrow(type = "closed", length = unit(2, "mm"))) +    
      geom_node_point(aes(size = strength), color = "darkred") +  
      scale_size_continuous(range = c(1, 8), name = "Weighted Degree Centrality") +  
      scale_edge_color_gradientn(colors = c("darkgray", "dodgerblue", "navy"), 
                                 values = scales::rescale(unique_weights), 
                                 name = "Edge Weight") +  
      guides(size = guide_legend(order = 1), 
             color = guide_colorbar(order = 2)) +
      theme_void() + 
      ggtitle("Village A") +  
      theme(text = element_text(size = 30),  
            plot.title = element_text(size = 30, face = "bold"),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 25, vjust = 3),
            legend.spacing = unit(2, "cm"))
      
      plot_sarahandrano <- ggraph(sarahandrano_net, layout = "stress") +  
        geom_edge_link(aes(color = weight), 
                       alpha = 1,  
                       arrow = arrow(type = "closed", length = unit(2, "mm"))) +    
        geom_node_point(aes(size = strength), color = "darkred") +  
        scale_size_continuous(range = c(1, 8), name = "Weighted Degree Centrality") +  
        scale_edge_color_gradientn(colors = c("darkgray", "dodgerblue", "navy"), 
                                   values = scales::rescale(unique_weights), 
                                   name = "Edge Weight") +  
        guides(size = guide_legend(order = 1), 
               color = guide_colorbar(order = 2)) +
        theme_void() + 
        ggtitle("Village B") +  
        theme(text = element_text(size = 30),
              plot.title = element_text(size = 30, face = "bold"),
              legend.text = element_text(size = 25),
              legend.title = element_text(size = 25, vjust = 3),
              legend.spacing = unit(2, "cm"))
      
      plot_andatsakala <- ggraph(andatsakala_net, layout = "stress") +  
        geom_edge_link(aes(color = weight), 
                       alpha = 1,  
                       arrow = arrow(type = "closed", length = unit(2, "mm"))) +    
        geom_node_point(aes(size = strength), color = "darkred") +  
        scale_size_continuous(range = c(1, 8), name = "Weighted Degree Centrality") +  
        scale_edge_color_gradientn(colors = c("darkgray", "dodgerblue", "navy"), 
                                   values = scales::rescale(unique_weights), 
                                   name = "Edge Weight") +
        guides(size = guide_legend(order = 1), 
               color = guide_colorbar(order = 2)) +
        theme_void() + 
        ggtitle("Village C") +  
        theme(text = element_text(size = 30),
              plot.title = element_text(size = 30, face = "bold"),
              legend.text = element_text(size = 25),
              legend.title = element_text(size = 25, vjust = 3),
              legend.spacing = unit(2, "cm"))
      
    # Combine the plots in a single column (3 rows)
      final_plot <- plot_mandena / plot_sarahandrano / plot_andatsakala
    
    # Display the combined plot
      print(final_plot)
    
    # Compute Descriptive Statistics for Summary Networks
      mean(strength(mandena_net))
      sd(strength(mandena_net))
      mean(strength(sarahandrano_net))
      sd(strength(sarahandrano_net))
      mean(strength(andatsakala_net))
      sd(strength(andatsakala_net))
      
######################################
#   CALCULATE CENTRALITY MEASURES    #
######################################

# Get Centrality Measures for Each Village
  centrality_mandena <- calculate_centrality(mandena_net)
  centrality_sarahandrano <- calculate_centrality(sarahandrano_net)
  centrality_andatsakala <- calculate_centrality(andatsakala_net)

# Combine All Centrality Measures
  centrality_all <- bind_rows(
    centrality_mandena,
    centrality_sarahandrano,
    centrality_andatsakala
  )

# Join Centrality Measures with Demographic Data
  demo_df <- demo_df %>%
    left_join(centrality_all, by = "social_netid")

#######################################
#   REFORMAT DATA FOR ROLE ANALYSIS   #
#######################################

# Create a List Object Containing Nodelist and Edgelist for Each Village
  net_data <- list(mandena = list(nl = nl_mandena,
                                  el = el_mandena),
                   sarahandrano = list(nl = nl_sarahandrano,
                                       el = el_sarahandrano),
                   andatsakala = list(nl = nl_andatsakala,
                                      el = el_andatsakala))

# Iterate Netwrite Function Over Data for Each Village
  netwrite_list <- list()
  for (i in names(net_data)) {
    village <- net_data[[i]]
    
    # Execute Netwrite Function
      output <- netwrite(nodelist = village$nl,
                         node_id = "social_netid",
                         i_elements = village$el$ego_id,
                         j_elements = village$el$alter_id,
                         type = village$el$relation,
                         directed = TRUE)
    
    # Add Netwrite Output to a List
      netwrite_list[[i]] <- list(
        network = output$network,
        igraph_list = output$igraph_list,
        largest_bi_component = output$largest_bi_component,
        largest_component = output$largest_component,
        node_measure_plot = output$node_measure_plot,
        node_measures = output$node_measures,
        edgelist = output$edgelist,
        system_level_measures = output$system_level_measures,
        system_measure_plot = output$system_measure_plot
    )
  }

####################
#   ROLE ANALYSIS  #
####################

# Initialize List to Store Output for Each Village
  role_analysis_list <- vector("list", length(netwrite_list))
  names(role_analysis_list) <- names(netwrite_list)

# Iterate Role Analysis Function Over Netwrite Object for Each Village
    for (i in seq_along(netwrite_list)) {
      village <- netwrite_list[[i]]

    # Execute Role Analysis Function
      output <- role_analysis(graph = village$igraph_list,
                              nodes = village$node_measures,
                              directed = TRUE,
                              method = "cluster",
                              fast_triad = TRUE,
                              min_partitions = 6,
                              max_partitions = 20,
                              min_partition_size = 5,
                              viz = TRUE,
                              retain_variables = TRUE,
                              cluster_summaries = TRUE)

    # Add Role Analysis Output to the list
      role_analysis_list[[i]] <- list(
        cluster_assignments = output$cluster_assignments,
        cluster_dendrogram = output$cluster_dendrogram,
        cluster_modularity = output$cluster_modularity,
        cluster_relations_heatmaps = output$cluster_relations_heatmaps,
        cluster_relations_sociogram = output$cluster_relations_sociogram,
        cluster_sociogram = output$cluster_sociogram,
        cluster_summaries = output$cluster_summaries,
        cluster_summaries_cent = output$cluster_summaries_cent,
        cluster_summaries_correlations = output$cluster_summaries_correlations,
        cluster_summaries_triad = output$cluster_summaries_triad,
        clustering_variables = output$clustering_variables
    )
    }

# Save Output
# saveRDS(role_analysis_list, "role_output.rds")
  # role_analysis_list <- read_rds("C:/Users/tmbar/OneDrive/Documents/GitHub/role_analysis/role_output.rds")
  # role_analysis_list <- read_rds("/Users/tylerbarrett/Documents/GitHub/role_analysis/role_output.rds")
  
# Plot Triad and Centrality Summaries
 # Assign Cluster Groups
   cluster_groups_man <- list(
     most_popular = c(7),
     core = c(1, 2, 5, 6, 8),
     periphery = c(3, 4)
   )
   cluster_groups_sara <- list(
     most_popular = c(2),
     core = c(1, 4),
     periphery = c(3, 5)
   )
   cluster_groups_andat <- list(
     most_popular = c(2),
     core = c(3),
     periphery = c(1,4)
   )
    man_summary_plots <- plot_role_summaries(role_analysis_list$mandena$clustering_variables, cluster_groups_man)
    sara_summary_plots <- plot_role_summaries(role_analysis_list$sarahandrano$clustering_variables, cluster_groups_sara)
    andat_summary_plots <- plot_role_summaries(role_analysis_list$andatsakala$clustering_variables, cluster_groups_andat)
    
# Examine Differences in Demographics by Role
  
  # Exctract Cluster Assignments and Join with Demographic Data
    net_data$mandena$nl <- net_data$mandena$nl %>%
      mutate(cluster_assignment = role_analysis_list$mandena$cluster_assignments$best_fit)
    net_data$sarahandrano$nl <- net_data$sarahandrano$nl %>%
      mutate(cluster_assignment = role_analysis_list$sarahandrano$cluster_assignments$best_fit)
    net_data$andatsakala$nl <- net_data$andatsakala$nl %>%
      mutate(cluster_assignment = role_analysis_list$andatsakala$cluster_assignments$best_fit)
    cluster_assignments <- net_data$mandena$nl %>%
      bind_rows(net_data$sarahandrano$nl, net_data$andatsakala$nl) %>%
      select(social_netid, cluster_assignment)
    demo_df <- demo_df %>%
      left_join(cluster_assignments)
  
  # Make Table 2
    demo_df <- demo_df %>%
      mutate(group = case_when(
        village == "Mandena" & cluster_assignment == 7 ~ "MostPopular",
        village == "Mandena" & cluster_assignment == 1 ~ "Core",
        village == "Mandena" & cluster_assignment == 2 ~ "Core",
        village == "Mandena" & cluster_assignment == 5 ~ "Core",
        village == "Mandena" & cluster_assignment == 6 ~ "Core",
        village == "Mandena" & cluster_assignment == 8 ~ "Core",
        village == "Mandena" & cluster_assignment == 3 ~ "Periphery",
        village == "Mandena" & cluster_assignment == 4 ~ "Periphery",
        village == "Sarahandrano" & cluster_assignment == 2 ~ "MostPopular",
        village == "Sarahandrano" & cluster_assignment == 1 ~ "Core",
        village == "Sarahandrano" & cluster_assignment == 4 ~ "Core",
        village == "Sarahandrano" & cluster_assignment == 3 ~ "Periphery",
        village == "Sarahandrano" & cluster_assignment == 5 ~ "Periphery",
        village == "Andatsakala" & cluster_assignment == 2 ~ "MostPopular",
        village == "Andatsakala" & cluster_assignment == 3 ~ "Core",
        village == "Andatsakala" & cluster_assignment == 1 ~ "Periphery",
        village == "Andatsakala" & cluster_assignment == 4 ~ "Periphery"
      ))
      
    demo_df %>%
      tbl_summary(
        include = c(age, gender, school_level, main_activity,
                    house_sol, goods_owned, household_size),
        by = group,  # Group by the new combined variable
        missing = "ifany"
      )
    
  # Compute Median (IQR) for Durable Goods Owned  
    demo_df %>%
      group_by(village) %>%
      summarise(
        median_goods = mean(goods_owned, na.rm = TRUE),
        Q1 = quantile(goods_owned, 0.25, na.rm = TRUE),
        Q3 = quantile(goods_owned, 0.75, na.rm = TRUE),
        IQR_goods = IQR(goods_owned, na.rm = TRUE)
      ) %>%
      bind_rows(
        demo_df %>%
          summarise(
            village = "All Villages",
            median_goods = mean(goods_owned, na.rm = TRUE),
            Q1 = quantile(goods_owned, 0.25, na.rm = TRUE),
            Q3 = quantile(goods_owned, 0.75, na.rm = TRUE),
            IQR_goods = IQR(goods_owned, na.rm = TRUE)
          )
      )
    
    demo_df %>%
      group_by(group) %>%
      summarise(
        median_goods = mean(goods_owned, na.rm = TRUE),
        Q1 = quantile(goods_owned, 0.25, na.rm = TRUE),
        Q3 = quantile(goods_owned, 0.75, na.rm = TRUE),
        IQR_goods = IQR(goods_owned, na.rm = TRUE)
      )

##################################################
#   ASSOCIATION BETWEEN ROLES AND VIRUS EXPOSURE #
##################################################

# Load Virscan Data
  virscan1 <- read_csv(paste0(fp, "/EEID_Data_public/clean_data_tables/Virscan/C5/C5_All_HitsList.csv"))
  virscan1 <- virscan1 %>%
    mutate(run = "C5")
  virscan2 <- read_csv(paste0(fp, "/EEID_Data_public/clean_data_tables/Virscan/C10/C10_All_HitsList.csv"))
  virscan2 <- virscan2 %>%
    mutate(run = "C10")
  virscan <- bind_rows(virscan1,virscan2)
  
# Count Number of Viruses in Panel
  length(unique(virscan$uniprot_specie)) # 337
  
# Simplify Data and Compute Virus Species Richness
  virscan <- virscan %>%
    mutate(social_netid = str_remove(sample_name, " FTA| W903")) %>%
    mutate(social_netid = sub("A-SNH", "A.SNH", social_netid)) %>%
    mutate(social_netid = sub("D-SNH", "D.SNH", social_netid)) %>%
    mutate(social_netid = sub("E-SNH", "E.SNH", social_netid)) %>%
    filter(grepl("SNH", social_netid)) %>%
    select(social_netid, run, uniprot_family, uniprot_specie, virus_pos) %>%
    group_by(social_netid, uniprot_specie) %>%
    mutate(virus_pos = if_else(any(virus_pos == TRUE), 1, 0)) %>%
    ungroup() %>%
    select(social_netid, run, uniprot_specie, virus_pos) %>%
    distinct() %>%
    pivot_wider(names_from = uniprot_specie,
                values_from = virus_pos,
                values_fill = list(virus_pos = 0)) %>%
    mutate(species_richness = rowSums(select(., -social_netid, -run), na.rm = TRUE))

# Join with Demographic Data
  demo_df <- demo_df %>%
    left_join(virscan, by = join_by(social_netid)) %>%
    filter(!is.na(species_richness))
  
# Characterize viral Exposure by Role Type
  # Relevel Groups
    demo_df <- demo_df %>%
      mutate(group = factor(group, levels = c("Periphery", "Core", "MostPopular")))
    
  # Plot Species Richness
    hist(demo_df$species_richness)
    ggplot(demo_df, aes(x = group, y = species_richness, color = village)) +
      geom_boxplot() +
      geom_point(position = position_jitter(width = 0.2)) +
      labs(x = "Group", y = "Species Richness") +
      theme_minimal() +
      facet_wrap(~ village, scales = "free")
  
  # Plot Species Prevalence by Role  
    # Both Villages Combined
      exposure_summary_all_villages_df <- demo_df %>%
        select(village, group, social_netid, 13:348, -cluster_assignment) %>% 
        mutate(across(-c(village, group, social_netid), as.numeric)) %>%
        pivot_longer(cols = -c(village, group, social_netid), names_to = "virus", values_to = "positive") %>%
        complete(village, group, virus, fill = list(positive = 0)) %>%
        group_by(village, group, virus) %>%
        summarise(count = sum(positive == 1, na.rm = TRUE), .groups = "drop") %>%
        left_join(
          demo_df %>%
            select(village, group, social_netid) %>%
            distinct() %>%
            group_by(village, group) %>%
            summarise(sample_size = n_distinct(social_netid), .groups = "drop"), 
          by = c("village", "group")
        ) %>%
        mutate(percentage = (count / sample_size) * 100) %>%
        group_by(virus) %>%
        filter(sum(percentage > 0) > 0) %>% 
        arrange(village, group, virus)
    
    # Count Total Viruses with ≥ 1 Positive Individual
      length(unique(exposure_summary_all_villages_df$virus))
      
      exposure_summary_all_villages <- ggplot(exposure_summary_all_villages_df, aes(x = group, y = virus, fill = percentage)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "maroon") +
        scale_x_discrete(labels = c("Periphery", "Core" = "Hangers-On", "MostPopular" = "Popular")) +
        theme_minimal() +
        labs(x = "Role Category",
             y = "Virus Species and Subtype",
             fill = "Seroprevalence (%)") +
        theme(axis.text = element_text(size = 24, color = "black"),
              axis.title = element_text(size = 30),
              legend.title = element_text(size = 30, margin = margin(b = 20)),
              legend.text = element_text(size = 22))
      
      # ggsave("species_prevalence_heatmap.png", exposure_summary_all_villages, width = 20, height = 20, dpi = 300)
      
    # Sarahandrano
      exposure_summary_sara_df <- exposure_summary_all_villages_df %>%
        filter(village == "Sarahandrano")
      
      ggplot(exposure_summary_sara_df, aes(x = group, y = virus, fill = percentage)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "maroon") +
        theme_minimal() +
        labs(
          title = "Viral Exposure by Role Type — Sarahandrano",
          x = "Role Type",
          y = "Virus Species",
          fill = "Percent Exposed"
        ) +
        theme(axis.text.x = element_text(hjust = 1))
      
    # Andatsakala
      exposure_summary_andat_df <- exposure_summary_all_villages_df %>%
        filter(village == "Andatsakala")
      
      ggplot(exposure_summary_andat_df, aes(x = group, y = virus, fill = percentage)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "maroon") +
        theme_minimal() +
        labs(
          title = "Viral Exposure by Role Type — Andatsakala",
          x = "Role Type",
          y = "Virus Species",
          fill = "Percent Exposed"
        ) +
        theme(axis.text.x = element_text(hjust = 1))
    
####################################
#   COMBINED EXPOSURE RISK MODEL   #
####################################

#	Pivot to Long Format
  model_df_1 <- demo_df %>%
    select(-species_richness) %>%
    pivot_longer(
      cols = 17:351,
      names_to = "virus",
      values_to = "infected"
    ) %>%
    mutate(group = as.factor(group))

#	Filter to Viruses with >10% Prevalence
#	Calculate prevalence
  virus_prevalence <- model_df_1 %>%
    group_by(virus) %>%
    summarize(prevalence = mean(infected, na.rm = TRUE)) %>%
    filter(prevalence > 0.10)

#	Apply filter
  model_df_1 <- model_df_1 %>%
    filter(virus %in% virus_prevalence$virus)

#	Transform Variables for Model
model_df_1 <- model_df_1 %>%
  mutate(
    infected = as.integer(infected),
    group_num = as.integer(factor(group, levels = c("Periphery", "Core", "MostPopular"))),
    virus_id = as.integer(factor(virus)),
    gender_num = as.integer(factor(gender, levels = c("Female", "Male"))),
    school_num = as.integer(factor(school_level)),
    activity_num = as.integer(factor(main_activity, levels = c("farmer", "non-farmer"))),
    village_id = as.integer(factor(village, levels = c("Andatsakala", "Sarahandrano"))),
    age_std = standardize(age),
    house_sol_std = standardize(house_sol),
    goods_std = standardize(goods_owned),
    hhsize_std = standardize(household_size)
  )
model_df_1 <- na.omit(model_df_1)

#	Select Variables for Model
  model_df_1 <- model_df_1 %>%
    select(infected, group_num, virus_id,
           age_std, house_sol_std, goods_std, hhsize_std,
           gender_num, school_num, activity_num)


#	Fit Multilevel Logistic Regression Using Rethinking Package
  m1 <- ulam(
    alist(
      infected ~ dbinom(1, p),
      logit(p) <- a +
        b_core*(group_num==2) +
        b_popular*(group_num==3) +
        b_age*age_std +
        b_gender[gender_num] +
        b_school[school_num] +
        b_activity[activity_num] +
        b_house*house_sol_std +
        b_goods*goods_std +
        b_hhsize*hhsize_std +
        v[virus_id],
      
      b_core ~ dnorm(0, 1),
      b_popular ~ dnorm(0, 1),
      b_age ~ dnorm(0, 0.5),
      b_house ~ dnorm(0, 0.5),
      b_goods ~ dnorm(0, 0.5),
      b_hhsize ~ dnorm(0, 0.5),
      b_gender[gender_num] ~ dnorm(0, 0.5),
      b_school[school_num] ~ dnorm(0, 0.5),
      b_activity[activity_num] ~ dnorm(0, 0.5),
      v[virus_id] ~ dnorm(0, sigma_v),
      sigma_v ~ dexp(1),
      a ~ dnorm(0, 1.5)
    ),
    data = model_df_1,
    chains = 4,
    cores = 4,
    iter = 2000,
    log_lik = TRUE
  )

# Model Summary
  precis(m1, depth = 2)

# Check Model Performance  
#	Trace Plots
  traceplot(m1, pars = c("b_core", "b_popular", "a", "sigma_v"))

#	Trace Rank Plots
  trankplot(m1, pars = c("b_core", "b_popular", "a", "sigma_v"))
  
#	Check Random Effects
  virus_effects <- data.frame(
    virus_id = 1:length(post$v[1,]),
    mean = apply(post$v, 2, mean),
    lower = apply(post$v, 2, quantile, 0.025),
    upper = apply(post$v, 2, quantile, 0.975)
  )
  print(virus_effects)
  p_virus <- ggplot(virus_effects, aes(x = reorder(virus_id, mean), y = mean)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = "Virus-specific Random Effects",
      subtitle = "Effect on log-odds (relative to average virus)",
      x = "Virus ID",
      y = "Effect on log-odds"
    ) +
    theme_minimal()
  print(p_virus)
  
#	Posterior Predictive Check
  #	Calculate Observed Exposure Rates
    obs_rates <- model_df_1 %>%
      mutate(group = case_when(
        group_num == 1 ~ "Periphery",
        group_num == 2 ~ "Core",
        group_num == 3 ~ "MostPopular"
      )) %>%
      group_by(group) %>%
      summarize(
        obs_rate = mean(infected),
        n = n()
      )
  #	Predicted Exposure Rates
    pred_rates <- data.frame(
      group = c("Periphery", "Core", "MostPopular"),
      pred_mean = c(mean(p.periphery), mean(p.core), mean(p.popular)),
      pred_lower = c(
        quantile(p.periphery, 0.025),
        quantile(p.core, 0.025),
        quantile(p.popular, 0.025)
      ),
      pred_upper = c(
        quantile(p.periphery, 0.975),
        quantile(p.core, 0.975),
        quantile(p.popular, 0.975)
      )
    )
  comparison <- merge(obs_rates, pred_rates, by = "group")
  print(comparison)
  p_pred <- ggplot(comparison, aes(x = group)) +
    geom_point(aes(y = obs_rate, color = "Observed"), size = 4) +
    geom_point(aes(y = pred_mean, color = "Predicted"), size = 4) +
    geom_errorbar(aes(ymin = pred_lower, ymax = pred_upper), width = 0.2, alpha = 0.5) +
    scale_color_manual(values = c("Observed" = "red", "Predicted" = "blue")) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Observed vs Predicted Infection Rates by Group",
      subtitle = "Blue points show model predictions with 95% CI",
      x = "Group",
      y = "Infection rate",
      color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  print(p_pred)

#	Extract Posterior Samples
  post <- extract.samples(m1)

#	Compute Group-Specific Probabilities
  #	Log-odds for each group
    mu.periphery <- post$a
    mu.core <- post$a + post$b_core
    mu.popular <- post$a + post$b_popular
  
  #	Convert to probability scale
    p.periphery <- inv_logit(mu.periphery)
    p.core <- inv_logit(mu.core)
    p.popular <- inv_logit(mu.popular)

#	Compute Pairwise Differences
  #	Core vs Periphery
    diff.core.periph.logodds <- post$b_core
    diff.core.periph.prob <- p.core - p.periphery
    
  #	MostPopular vs Periphery
    diff.popular.periph.logodds <- post$b_popular
    diff.popular.periph.prob <- p.popular - p.periphery
  
  #	Core vs MostPopular
    diff.core.popular.logodds <- post$b_core - post$b_popular
    diff.core.popular.prob <- p.core - p.popular

#	Summary Statistics
  summary_stats <- precis(data.frame(
    p_periphery = p.periphery,
    p_core = p.core,
    p_popular = p.popular,
    diff_core_v_periph = diff.core.periph.prob,
    diff_popular_v_periph = diff.popular.periph.prob,
    diff_core_v_popular = diff.core.popular.prob
  ))
  print(summary_stats)

#	Group Probabilities Table
  summary_table <- data.frame(
    Group = c("Periphery (ref)", "Core", "MostPopular"),
    Mean_Prob = round(c(mean(p.periphery), mean(p.core), mean(p.popular)), 3),
    Median_Prob = round(c(median(p.periphery), median(p.core), median(p.popular)), 3),
    SD = round(c(sd(p.periphery), sd(p.core), sd(p.popular)), 3),
    Lower_95 = round(c(
      quantile(p.periphery, 0.025),
      quantile(p.core, 0.025),
      quantile(p.popular, 0.025)
    ), 3),
    Upper_95 = round(c(
      quantile(p.periphery, 0.975),
      quantile(p.core, 0.975),
      quantile(p.popular, 0.975)
    ), 3)
  )
  print(summary_table)

#	Pairwise Comparisons Table
  comparison_table <- data.frame(
    Comparison = c("Core - Periphery", "MostPopular - Periphery", "Core - MostPopular"),
    Mean_Diff = round(c(
      mean(diff.core.periph.prob),
      mean(diff.popular.periph.prob),
      mean(diff.core.popular.prob)
    ), 3),
    Lower_95 = round(c(
      quantile(diff.core.periph.prob, 0.025),
      quantile(diff.popular.periph.prob, 0.025),
      quantile(diff.core.popular.prob, 0.025)
    ), 3),
    Upper_95 = round(c(
      quantile(diff.core.periph.prob, 0.975),
      quantile(diff.popular.periph.prob, 0.975),
      quantile(diff.core.popular.prob, 0.975)
    ), 3),
    Prob_Greater_Zero = round(c(
      mean(diff.core.periph.prob > 0),
      mean(diff.popular.periph.prob > 0),
      mean(diff.core.popular.prob > 0)
    ), 3)
  )
  print(comparison_table)

#	MAIN FIGURE: TWO-PANEL PLOT

#	Panel A: Posterior Distributions by Group
  #	Prepare Data
    plot_data <- data.frame(
      value = c(p.periphery, p.core, p.popular),
      group = factor(
        rep(c("Periphery", "Hangers-On", "Popular"), each = length(p.periphery)),
        levels = c("Periphery", "Hangers-On", "Popular")
      )
    )
  
  #	Build Plot
    panel_a <- ggplot(plot_data, aes(x = value, fill = group)) +
      geom_density(alpha = 0.6) +
      scale_fill_brewer(palette = "Dark2") +
      scale_x_continuous(limits = c(0, 0.7)) +
      labs(
        title = "A) Posterior Probability of Exposure by Role Category",
        x = "Probability of Exposure",
        y = "Posterior Density",
        fill = "Role Category"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24)
    )

#	Panel B: Pairwise Group Differences
  #	Prepare Data
    diff_data <- data.frame(
      value = c(diff.core.periph.prob, diff.popular.periph.prob, diff.core.popular.prob),
      comparison = factor(
        rep(
          c("Hangers-On - Periphery", "Popular - Periphery", "Hangers-On - Popular"),
          each = length(diff.core.periph.prob)
        ),
        levels = c("Hangers-On - Periphery", "Popular - Periphery", "Hangers-On - Popular")
      )
    )
    
  #	Build Plot
    panel_b <- ggplot(diff_data, aes(x = value)) +
      geom_density(alpha = 0.6, fill = "navy") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
      facet_wrap(~comparison, ncol = 1, scales = "free_y") +
      scale_x_continuous(limits = c(-0.1, 0.25)) +
      labs(
        title = "B) Posterior Distributions of Role Category Differences",
        x = "Difference in Exposure Probability",
        y = "Posterior Density"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        strip.text = element_text(size = 24)
      )

#	Combine Panels
  combined_figure <- panel_a + panel_b +
    plot_layout(ncol = 2, widths = c(1, 1.2))
  print(combined_figure)

#	Save
  # ggsave(
  #   "role_exposure_prob.png",
  #   combined_figure,
  #   width = 20,
  #   height = 10,
  #   dpi = 300
  # )

#####################################
#   Species/Subtype Richness Model  #
#####################################
    
# Prepare Data for Modeling    
  demo_df <- demo_df %>%
    mutate(school_level = factor(school_level, levels = c("None", "Primary", "Secondary", "Higher")))
  model_df_2 <- demo_df %>%
    select(social_netid, village, age, gender, school_level, main_activity,
           house_sol, goods_owned, household_size, weighted_degree, betweenness, eigenvector, closeness, group, species_richness)
  model_df_2 <- na.omit(model_df_2)
    
# Standardize Continuous Predictor Variables
  model_df_2 <- model_df_2 %>%  
    mutate(age = datawizard::standardize(age)) %>%
    mutate(goods_owned = datawizard::standardize(goods_owned)) %>%
    mutate(household_size = datawizard::standardize(household_size)) %>%
    mutate(weighted_degree = datawizard::standardize(weighted_degree)) %>%
    mutate(betweenness = datawizard::standardize(betweenness)) %>%
    mutate(eigenvector = datawizard::standardize(eigenvector)) %>%
    mutate(closeness = datawizard::standardize(closeness))

# Specify Global Model
  m_viruses <- glmmTMB(species_richness ~ age + gender + school_level + main_activity + 
                         house_sol + goods_owned + household_size + village +
                         weighted_degree + betweenness + eigenvector + closeness + group,
                       data = model_df_2,
                       family = poisson,
                       ziformula = ~1,
                       na.action = na.fail)
  summary(m_viruses)
  performance::check_model(m_viruses)
  DHARMa::testZeroInflation(m_viruses)

# Use Dredge for Model Comparison
  m_viruses_dredge <- dredge(m_viruses)

# Average Models With Delta AICc < 2
  m_viruses_avg <- model.avg(m_viruses_dredge, subset = delta < 2)
  summary(m_viruses_avg)
  plot(m_viruses_avg, intercept = FALSE)
  sw(m_viruses_avg)

# Plot Model Averaging Results # note -- add color gradiant for importance
# Create Dataframe for Plotting
  m_viruses_summary <- summary(m_viruses_avg)
  plot_df_richness <- as.data.frame(m_viruses_summary$coefmat.full)
  plot_df_richness <- plot_df_richness %>%
    mutate(CI.min = Estimate - 1.96 * `Std. Error`) %>%
    mutate(CI.max = Estimate + 1.96 * `Std. Error`)
  plot_df_richness <- rownames_to_column(plot_df_richness, "coefficient")
  names(plot_df_richness) <- gsub(" ", "", names(plot_df_richness))
  plot_df_richness <- plot_df_richness %>%
    filter(coefficient != "(Intercept)") %>%
    mutate(coefficient = recode(coefficient,
                                "cond(age)" = "Age",
                                "cond(genderMale)" = "Gender: Men (Ref: Women)",
                                "cond(school_levelPrimary)" = "Education: Primary (Ref: None)",
                                "cond(school_levelSecondary)" = "Education: Secondary (Ref: None)",
                                "cond(school_levelHigher)" = "Education: Higher (Ref: None)",
                                "cond(house_sol)" = "Household Lifestyle Index",
                                "cond(goods_owned)" = "Durable Goods Owned",
                                "cond(villageSarahandrano)" = "Village: B (Ref: C)",
                                "cond(weighted_degree)" = "Strength Centrality",
                                "cond(eigenvector)" = "Eigenvector Centrality",
                                "cond(closeness)" = "Closeness Centrality",
                                "cond(groupCore)" = "Role: Hangers-On (Ref: Periphery)",
                                "cond(groupMostPopular)" = "Role: Popular (Ref: Periphery)"))

# Add Variable Importance to Plot Dataframe
  plot_df_richness <- plot_df_richness %>%
    mutate(importance = case_when(
      coefficient == "Role: Hangers-On (Ref: Periphery)" ~ "1",
      coefficient == "Role: Popular (Ref: Periphery)" ~ "1",
      coefficient == "Strength Centrality" ~ "0.15",
      coefficient == "Eigenvector Centrality" ~ "0.64",
      coefficient == "Closeness Centrality" ~ "0.07",
      coefficient == "Education: Primary (Ref: None)" ~ "1",
      coefficient == "Education: Secondary (Ref: None)" ~ "1",
      coefficient == "Education: Higher (Ref: None)" ~ "1",
      coefficient == "Village: B (Ref: C)" ~ "1",
      coefficient == "Age" ~ "0.17",
      coefficient == "Household Lifestyle Index" ~ "0.19",
      coefficient == "Durable Goods Owned" ~ "0.07",
      coefficient == "Gender: Men (Ref: Women)" ~ "0.08",
      TRUE ~ "Unknown"
    )) %>%
    mutate(importance = as.numeric(importance))
  
  coef_plot <- ggplot(plot_df_richness, aes(x = coefficient, y = Estimate, ymin = CI.min, ymax = CI.max)) +
    geom_hline(yintercept=0, lty=2) +
    geom_errorbar(aes(ymin = CI.min, ymax = CI.max), size = 2, width = 0.2) +
    geom_point(aes(x = fct_rev(coefficient), color = importance), size = 8, alpha = 100) +
    geom_point(shape = 1, size = 8, stroke = 1.5, color = "black") +
    scale_color_gradient(name = "Importance\n(AICc Weight)", low = "lightblue", high = "darkblue", limits = c(0, 1),
                         guide = guide_colorbar(title.vjust = 3)) +
    coord_flip() +
    xlab("") + ylab("Coefficient (95% Confidence Interval)") + 
    scale_x_discrete(limits = c("Role: Popular (Ref: Periphery)", "Role: Hangers-On (Ref: Periphery)",
                                "Closeness Centrality", "Eigenvector Centrality", "Strength Centrality",
                                "Village: B (Ref: C)", "Household Lifestyle Index", "Durable Goods Owned",
                                "Education: Higher (Ref: None)",
                                "Education: Secondary (Ref: None)", "Education: Primary (Ref: None)",
                                "Gender: Men (Ref: Women)", "Age")) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 30, color = "black"),
          axis.text.x = element_text(size = 30, color = "black"),
          axis.title.y = element_text(size = 30),
          axis.title.x = element_text(size = 30),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, "cm"),
          legend.position = "right",
          legend.box = "vertical",
          legend.box.just = "left")

# ggsave("coefficient_plot.png",
#        plot = coef_plot,
#        width = 20,
#        height = 15,
#        dpi = 600,
#        bg = "white")    
  
  