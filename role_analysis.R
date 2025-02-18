# Role Analysis of Madagascar NIH Social Network Data
# Created: September 07, 2023
# Tyler Barrett


#######################################
#   LOAD PACKAGES AND SET FILE PATH   #
#######################################

# Load Packages
  library(tidyverse)
  library(gtsummary)
  library(igraph)
  library(ggraph)
  library(patchwork)
  library(ideanet)
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

#########################################
#   CREATE NODE LIST FOR EACH VILLAGE   #
#########################################

# Read in Demographic Data
  demo_df <- read_csv(paste0(fp, "/EEID_Data_public/clean_data_tables/Survey_Demographic_Health.csv"))
  
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
  
# Select Relevant Variables - Just Basic Demographics for Now
  demo_df <- demo_df %>%
    select(social_netid, village, gender, age, school_level, main_activity,
           house_sol, goods_owned, household_size)

# Recode Ampandrana as Andatsakala
  demo_df <- demo_df %>%
    mutate(village = case_when(
      village == "Ampandrana" ~ "Andatsakala",
      TRUE ~ village
    ))

# Separate Node Lists for Each Village
  nl_mandena <- demo_df %>%
    filter(village == "Mandena")
  nl_sarahandrano <- demo_df %>%
    filter(village == "Sarahandrano")
  nl_andatsakala <- demo_df %>%
    filter(village == "Andatsakala")

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
   write.csv(net_summaries, "net_summaries.csv")
    
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
 role_analysis_list <- read_rds("C:/Users/tmbar/OneDrive/Documents/GitHub/role_analysis/role_output.rds")
 role_analysis_list <- read_rds("/Users/tylerbarrett/Documents/GitHub/role_analysis/role_output.rds")

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
  
# characterize viral Exposure by Role Type
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
        select(group, social_netid, c(13:348)) %>% 
        pivot_longer(cols = -c(group, social_netid), names_to = "virus", values_to = "positive") %>%
        complete(group, virus, fill = list(positive = 0)) %>%
        group_by(group, virus) %>%
        summarise(count = sum(positive == 1, na.rm = TRUE), .groups = "drop") %>%
        left_join(
          demo_df %>%
            select(group, social_netid) %>%
            distinct() %>%
            group_by(group) %>%
            summarise(sample_size = n_distinct(social_netid)), 
          by = "group"
        ) %>%
        mutate(percentage = (count / sample_size) * 100) %>%
        group_by(virus) %>%
        filter(sum(percentage > 0) > 0) %>% 
        arrange(group, virus)
      
      ggplot(exposure_summary_all_villages_df, aes(x = group, y = virus, fill = percentage)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "maroon") +
        theme_minimal() +
        labs(title = "Viral Exposure by Role Type",
             x = "Role Type",
             y = "Virus Species",
             fill = "Percent Exposed") +
        theme(axis.text.x = element_text(hjust = 1))
      
      # Sarahandrano
        exposure_summary_sara_df <- demo_df %>%
          filter(village == "Sarahandrano") %>%
          select(group, social_netid, c(9:343)) %>% 
          pivot_longer(cols = -c(group, social_netid), names_to = "virus", values_to = "positive") %>%
          complete(group, virus, fill = list(positive = 0)) %>%
          group_by(group, virus) %>%
          summarise(count = sum(positive == 1, na.rm = TRUE), .groups = "drop") %>%
          left_join(
            demo_df %>%
              select(group, social_netid) %>%
              distinct() %>%
              group_by(group) %>%
              summarise(sample_size = n_distinct(social_netid)), 
            by = "group"
          ) %>%
          mutate(percentage = (count / sample_size) * 100) %>%
          group_by(virus) %>%
          filter(sum(percentage > 0) > 0) %>% 
          arrange(group, virus)
        ggplot(exposure_summary_sara_df, aes(x = group, y = virus, fill = percentage)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "maroon") +
          theme_minimal() +
          labs(title = "Viral Exposure by Role Type",
               x = "Role Type",
               y = "Virus Species",
               fill = "Percent Exposed") +
          theme(axis.text.x = element_text(hjust = 1))
        
      # Andatsakala  
        exposure_summary_andat_df <- demo_df %>%
          filter(village == "Andatsakala") %>%
          select(group, social_netid, c(9:343)) %>% 
          pivot_longer(cols = -c(group, social_netid), names_to = "virus", values_to = "positive") %>%
          complete(group, virus, fill = list(positive = 0)) %>%
          group_by(group, virus) %>%
          summarise(count = sum(positive == 1, na.rm = TRUE), .groups = "drop") %>%
          left_join(
            demo_df %>%
              select(group, social_netid) %>%
              distinct() %>%
              group_by(group) %>%
              summarise(sample_size = n_distinct(social_netid)), 
            by = "group"
          ) %>%
          mutate(percentage = (count / sample_size) * 100) %>%
          group_by(virus) %>%
          filter(sum(percentage > 0) > 0) %>% 
          arrange(group, virus)
        ggplot(exposure_summary_andat_df, aes(x = group, y = virus, fill = percentage)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "maroon") +
          theme_minimal() +
          labs(title = "Viral Exposure by Role Type",
               x = "Role Type",
               y = "Virus Species",
               fill = "Percent Exposed") +
          theme(axis.text.x = element_text(hjust = 1))

# Compare Differences in Prevalence for Each Virus Across Roles
# Correct for Multiple Comparisons
  
# Species Richness Model
  demo_df <- demo_df %>%
    mutate(school_level = factor(school_level, levels = c("None", "Primary", "Secondary", "Higher")))
  
  model_df <- demo_df %>%
    select(social_netid, village, age, gender, school_level, main_activity,
           house_sol, goods_owned, household_size, group, species_richness)
  model_df = na.omit(model_df)
  
  # Standardize Continuous Predictor Variables
    model_df <- model_df %>%  
      mutate(age = datawizard::standardize(age)) %>%
      mutate(goods_owned = datawizard::standardize(goods_owned)) %>%
      mutate(household_size = datawizard::standardize(household_size))
  
  # Specify Global Model
    m_viruses <- glm(species_richness ~ age + gender + school_level + main_activity + 
                     house_sol + goods_owned + household_size + village + group,
               data = model_df,
               family = "poisson",
               na.action = na.fail)
    summary(m_viruses)
    performance::check_model(m_viruses)
  
  # Use Dredge for Model Comparison
    m_viruses_dredge <- dredge(m_viruses)
  
  # Average Models With Delta AICc < 2
    m_viruses_avg <- model.avg(m_viruses_dredge, subset = delta < 2)
    summary(m_viruses_avg)
    plot(m_viruses_avg, intercept = FALSE)

#######################
#   HOOKWORM ANALYSIS #
####################### 
  
# # Prepare Data for Analysis and Join with Demographic Data
#   hookworm_df <- read_csv("C:/Users/tmbar/Box/EEID_Data_public/clean_data_tables/HUMAN_PARASITE_ALL_VILLAGE.csv")
#   hookworm_df <- hookworm_df %>%
#     select(social_netid, NC_reads, Necator_americanus, Ancylostoma_ceylanicum) %>%
#     filter(NC_reads >= 500) %>% # filter based on 500 read cutoff
#     mutate(social_netid = sub("A.SNH.", "A.SNH", social_netid)) %>%
#     mutate(social_netid = sub("D.SNH.", "D.SNH", social_netid)) %>%
#     mutate(social_netid = sub("E.SNH.", "E.SNH", social_netid)) %>%
#     mutate(human_hookworm = if_else(Necator_americanus > 0, 1, 0)) %>%
#     mutate(dog_hookworm = if_else(Ancylostoma_ceylanicum > 0, 1, 0)) %>%
#     select(social_netid, human_hookworm, dog_hookworm)
#   demo_df <- demo_df %>%
#     left_join(hookworm_df, by = "social_netid")
# 
# # Load Animal Interaction Data to Get Dog Ownership & Join with Demographic Data
#   animal_df <- read_csv("C:/Users/tmbar/Box/EEID_Data_public/clean_data_tables/Survey_Animal_Interaction.csv")
#   animal_df <- animal_df %>%
#     select(social_netid, pet_dogs)
#   demo_df <- demo_df %>%
#     left_join(animal_df)
# 
# # Relevel Education Variable
#   demo_df <- demo_df %>%
#     mutate(school_level = factor(school_level, levels = c("None", "Primary", "Secondary", "Higher"))) %>%
#     mutate(school_level = recode(school_level, "None" = "< Secondary", "Primary" = "< Secondary",
#                                  "Secondary" = "≥ Secondary", "Higher" = "≥ Secondary"))
# 
# # Relevel Groups
#   demo_df <- demo_df %>%
#     mutate(group = factor(group, levels = c("Periphery", "Core", "MostPopular")))
#   
#   demo_df <- demo_df %>%
#     mutate(village = factor(village, levels = c("Mandena", "Sarahandrano", "Andatsakala")))
#   
#   demo_df <- demo_df %>%
#     mutate(material_floor = factor(material_floor, level = c("dirt", "rafia", "ravenala",
#                                                              "bamboo", "wood_planks", "cement"))) %>%
#     mutate(material_floor = recode(material_floor, "rafia" = "ravenala"))
#  
# # Compute Prevalence by Role Group
#   prevalence_data <- demo_df %>%
#     group_by(group) %>%
#     summarise(
#       human_hookworm_prevalence = mean(human_hookworm, na.rm = TRUE),
#       dog_hookworm_prevalence = mean(dog_hookworm, na.rm = TRUE),
#       coinfection_prevalence = mean(human_hookworm & dog_hookworm, na.rm = TRUE)
#     )
#   prevalence_data_long <- prevalence_data %>%
#     pivot_longer(cols = -group, names_to = "infection_type", values_to = "prevalence")
#   ggplot(prevalence_data_long, aes(x = group, y = prevalence, fill = infection_type)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     labs(
#       title = "Prevalence of Hookworm Infections by Group",
#       x = "Group",
#       y = "Prevalence",
#       fill = "Infection Type"
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
# # Model the Relationship Between Roles and Hookworm Infection
# 
# # Remove NAs for Modeling
#   model_df <- na.omit(demo_df)
#   
# # Standardize Continuous Variables
#   model_df <- model_df %>% 
#     mutate(landsize_in_daba = datawizard::standardize(landsize_in_daba)) %>%
#     mutate(household_size = datawizard::standardize(household_size)) %>%
#     mutate(age = datawizard::standardize(age))
#   
# # Build Global Model for Human Hookworm
#   m_human <- glm(human_hookworm ~ age + gender + school_level +
#                  main_activity + household_size + material_floor + landsize_in_daba + village + group,
#                 data = model_df,
#                 family = "binomial",
#                 na.action = na.fail)
#   summary(m_human)
#   performance::check_model(m_human)
#   
# # Use Dredge for Model Comparison
#   m_human_dredge <- dredge(m_human)
#   
# # Average Models With Delta AICc < 2
#   m_human_avg <- model.avg(m_human_dredge, subset = delta < 2)
#   summary(m_human_avg)
#   plot(m_human_avg, intercept = FALSE)
#   
# # Build Global Model for Human Hookworm
#   m_dog <- glm(dog_hookworm ~ age + gender + school_level + main_activity + pet_dogs + 
#                  household_size + material_floor + landsize_in_daba + village + group,
#                  data = model_df,
#                  family = "binomial",
#                
#                  na.action = na.fail)
#   summary(m_dog)
#   performance::check_model(m_dog)
#   
# # Use Dredge for Model Comparison
#   m_dog_dredge <- dredge(m_dog)
#   
# # Average Models With Delta AICc < 2
#   m_dog_avg <- model.avg(m_dog_dredge, subset = delta < 2)
#   summary(m_dog_avg)
#   plot(m_dog_avg, intercept = FALSE)
#   
#   all_dog <- glm(dog_hookworm ~
#                    age +
#                    gender +
#                    school_level +
#                    pet_dogs +
#                    village +
#                    group,
#                  data = model_df,
#                  family = "binomial")
#   summary(all_dog)

