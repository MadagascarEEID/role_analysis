# Role Analysis of Madagascar NIH Social Network Data
# Created: September 07, 2023
# Tyler Barrett


#######################################
#   LOAD PACKAGES AND SET FILE PATH   #
#######################################

# Load Packages
  library(tidyverse)
  library(ideanet)

# set File Path
  if (paste(Sys.info()[[1]], collapse=" ") == "Windows"){
  fp <- "C:/Users/tmbar/Box"
}else{
  fp <- "/Users/tylerbarrett/Library/CloudStorage/Box-Box"
}

#################
#   FUNCTIONS   #
#################

# Prevalence Computer
  prevalence <- function(data, parasite, group_type = NULL, group_name = NULL){
  
  # compute overall prevalence if group argument is missing
  
  if(is.null(group_type)) {
    
    result <- data %>%
      group_by(!!sym(parasite)) %>%
      summarize(count = n())
  }
  
  # filter by subgroup if specified
  
  else{
    result <- data %>%
      filter(!!sym(group_type) == !!(group_name)) %>%
      group_by(!!sym(parasite)) %>%
      summarize(count = n())
  }
  
  # print infection count and prevalence
  
  total_count <- sum(result$count)
  
  result <- result %>%
    mutate(prevalence = count / total_count * 100)
  
  cat(parasite, group_type, group_name)
  
  print(result)
}

# Make Degree Centrality Summary Plots
  degree_plot <- function(summary_df, cluster_list) {
  
  # make dataframe with mean degree
  degree_df <- summary_df %>%
    select(cluster, mean_in_degree_std, mean_out_degree_std,
           mean_freetime_in_degree_std, mean_freetime_out_degree_std,
           mean_farm_help_received_in_degree_std, mean_farm_help_received_out_degree_std,
           mean_farm_help_provided_in_degree_std, mean_farm_help_provided_out_degree_std,
           mean_food_help_received_in_degree_std, mean_food_help_received_out_degree_std,
           mean_food_help_provided_in_degree_std, mean_food_help_provided_out_degree_std) %>%
    pivot_longer(cols = -cluster, names_to = "centrality", values_to = "mean_std") %>%
    mutate(centrality = str_remove(centrality, "_std")) %>%
    mutate(centrality = str_remove(centrality, "mean_")) %>%
    mutate(color = if_else(mean_std > 0, "positive", "negative"))
  
  # create list to store plots
  degree_plot_list <- lapply(cluster_list, function(i) {
    
    # create cluster dataframe
    cluster_df <- degree_df %>%
      filter(cluster == i)
    
    # make plot
    p <- ggplot(cluster_df, aes(x = centrality, y = mean_std, fill = color)) +
      geom_col() +
      scale_fill_manual(values = c("negative" = "maroon", "positive" = "navy")) +
      xlab("Centrality Measure") +
      ylab("Mean Score (Standardized)") +
      coord_flip() +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(color = "black"))
    
    return(p)
  })
  
  # name list elements
  names(degree_plot_list) <- paste0("cluster_", seq_along(degree_plot_list))
  
  return(degree_plot_list)
}

# Make Triad Summary Plots
  triad_plot <- function(summary_df, cluster_list) {
  
  # make dataframe with mean degree
  triad_df <- summary_df %>%
    select(cluster, c(mean_summary_graph_021c_b_std:mean_summary_graph_300_std)) %>%
    pivot_longer(cols = -cluster, names_to = "triad", values_to = "mean_std") %>%
    mutate(triad = str_remove(triad, "mean_summary_graph_")) %>%
    mutate(triad = str_remove(triad, "_std")) %>%
    mutate(color = if_else(mean_std > 0, "positive", "negative"))
  
  # create list to store plots
  triad_plot_list <- lapply(cluster_list, function(i) {
    
    # create cluster dataframe
    cluster_df <- triad_df %>%
      filter(cluster == i)
    
    # make plot
    p <- ggplot(cluster_df, aes(x = triad, y = mean_std, fill = color)) +
      geom_col() +
      scale_fill_manual(values = c("negative" = "maroon", "positive" = "navy")) +
      xlab("Triad Position") +
      ylab("Mean Frequency (Standardized)") +
      coord_flip() +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(color = "black"))
    
    return(p)
  })
  
  # name list elements
  names(triad_plot_list) <- paste0("cluster_", seq_along(triad_plot_list))
  
  return(triad_plot_list)
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

# Select Relevant Variables - Just Basic Demographics for Now
  demo_df <- demo_df %>%
    select(social_netid, village, gender, age, school_level, main_activity)

# Separate Node Lists for Each Village
  nl_mandena <- demo_df %>%
    filter(village == "Mandena")
  nl_sarahandrano <- demo_df %>%
    filter(village == "Sarahandrano")
  nl_andatsakala <- demo_df %>%
    filter(village == "Andatsakala" | village == "Ampandrana")

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

# Set Clustering Parameters
min = 6 # Minimum Number of Partitions
max = 20 # Maximum Number of Partitions

# Initialize List to Store Output for Each Village
  role_analysis_list <- vector("list", length(netwrite_list))

# Iterate Role Analysis Function Over Netwrite Object for Each Village
    for (i in seq_along(netwrite_list)) {
      village <- netwrite_list[[i]]
    
    # Execute Role Analysis Function
      output <- role_analysis(graph = village$igraph_list,
                              nodes = village$node_measures,
                              directed = TRUE,
                              method = "cluster",
                              min_partitions = min,
                              max_partitions = max,
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
  
# loop over each village and extract summary edgelist
villages <- c("mandena", "sarahandrano", "andatsakala")
summary_el <- list()
for (i in seq_along(villages)) {
  
  # extract summary edgelist
  results <- summary_el_extracter(i)
  
  # add output to list
  summary_el[[villages[i]]] <- list(
    cluster_n = results$cluster_n,
    el = results$el
    )
}

# write out summary edgelists to plot by hand
write_csv(summary_el$mandena$el, "man_el.csv")
write_csv(summary_el$sarahandrano$el, "sara_el.csv")
write_csv(summary_el$andatsakala$el, "andat_el.csv")

# plot triad summaries
man_triad_plots <- triad_plot(role_analysis_list$mandena$cluster_summaries,
                              c(1, 2, 3, 4, 5, 6))
sara_triad_plots <- triad_plot(role_analysis_list$sarahandrano$cluster_summaries,
                              c(1, 2, 3, 4))
andat_triad_plots <- triad_plot(role_analysis_list$andatsakala$cluster_summaries,
                               c(1, 2, 3, 4))

# plot degree centrality summaries
man_degree_plots <- degree_plot(role_analysis_list$mandena$cluster_summaries,
                              c(1, 2, 3, 4, 5, 6))
sara_degree_plots <- degree_plot(role_analysis_list$sarahandrano$cluster_summaries,
                               c(1, 2, 3, 4))
andat_degree_plots <- degree_plot(role_analysis_list$andatsakala$cluster_summaries,
                                c(1, 2, 3, 4))

# arrange plots into grids for presentation
man_summary_plot_1 <- gridExtra::grid.arrange(grobs = list(man_triad_plots$cluster_1, man_triad_plots$cluster_2, man_triad_plots$cluster_3,
                                                        man_degree_plots$cluster_1, man_degree_plots$cluster_2, man_degree_plots$cluster_3),
                                              ncol = 3, nrow = 2)
man_summary_plot_2 <- gridExtra::grid.arrange(grobs = list(man_triad_plots$cluster_4, man_triad_plots$cluster_5, man_triad_plots$cluster_6,
                                                           man_degree_plots$cluster_4, man_degree_plots$cluster_5, man_degree_plots$cluster_6),
                                              ncol = 3, nrow = 2)
sara_summary_plot <- gridExtra::grid.arrange(grobs = list(sara_triad_plots$cluster_1, sara_triad_plots$cluster_2, sara_triad_plots$cluster_3,
                                                           sara_degree_plots$cluster_1, sara_degree_plots$cluster_2, sara_degree_plots$cluster_3),
                                              ncol = 3, nrow = 2)
andat_summary_plot <- gridExtra::grid.arrange(grobs = list(andat_triad_plots$cluster_1, andat_triad_plots$cluster_2, andat_triad_plots$cluster_3,
                                                            andat_degree_plots$cluster_1, andat_degree_plots$cluster_2, andat_degree_plots$cluster_3),
                                               ncol = 3, nrow = 2)

# save plots
ggsave("man_summary_plot_1.pdf", man_summary_plot_1, width = 20, height = 10)
ggsave("man_summary_plot_2.pdf", man_summary_plot_2, width = 20, height = 10)
ggsave("sara_summary_plot.pdf", sara_summary_plot, width = 20, height = 10)
ggsave("andat_summary_plot.pdf", andat_summary_plot, width = 20, height = 10)

#######################################
## Do roles predict hookworm infection?
#######################################

# read in hookworm data
hookworm_df <- read_csv("/Users/tylerbarrett/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/HUMAN_PARASITE_ALL_VILLAGE.csv")

# select relevant data and prep data
hookworm_df <- hookworm_df %>%
  select(social_netid, NC_reads, Necator_americanus, Ancylostoma_ceylanicum) %>%
  filter(NC_reads >= 500) %>% # filter based on 500 read cutoff
  mutate(social_netid = sub("ASNH_", "A.SNH", social_netid)) %>% # change "_" to "."
  mutate(social_netid = sub("DSNH_", "D.SNH", social_netid)) %>%
  mutate(social_netid = sub("ESNH_", "E.SNH", social_netid))

# merge with demographic data
hookworm_df <- hookworm_df %>%
  left_join(demo_df, by = "social_netid")

# combine Andatsakala and Ampandrana
hookworm_df <- hookworm_df %>%
  mutate(village = if_else(village == "Ampandrana", "Andatsakala", village))

# create infection indicator
hookworm_df <- hookworm_df %>%
  mutate(human_hookworm = if_else(Necator_americanus > 0, 1, 0)) %>%
  mutate(dog_hookworm = if_else(Ancylostoma_ceylanicum > 0, 1, 0))

# extract cluster assignments
net_data$mandena$nl <- net_data$mandena$nl %>%
  mutate(cluster_assignment = role_analysis_list$mandena$cluster_assignments$best_fit)
net_data$sarahandrano$nl <- net_data$sarahandrano$nl %>%
  mutate(cluster_assignment = role_analysis_list$sarahandrano$cluster_assignments$best_fit)
net_data$andatsakala$nl <- net_data$andatsakala$nl %>%
  mutate(cluster_assignment = role_analysis_list$andatsakala$cluster_assignments$best_fit)

# join into one dataframe
cluster_assignments <- net_data$mandena$nl %>%
  bind_rows(net_data$sarahandrano$nl, net_data$andatsakala$nl) %>%
  select(social_netid, cluster_assignment)

# add cluster assignments to hookworm dataframe
hookworm_df <- hookworm_df %>%
  left_join(cluster_assignments, by = "social_netid")

animal_df <- read_csv("/Users/tylerbarrett/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/Survey_Animal_Interaction.csv")

animal_df <- animal_df %>%
  select(social_netid, pet_dogs)

hookworm_df <- hookworm_df %>%
  left_join(animal_df)

# filter to complete cases for modeling
hookworm_df <- hookworm_df %>%
  filter(!is.na(age) & !is.na(gender) & !is.na(school_level), !is.na(pet_dogs))

# standardize numeric variables
hookworm_df <- hookworm_df %>%
  mutate(age = scale(age))

# relevel education for models
hookworm_df <- hookworm_df %>%
  mutate(school_level = factor(school_level, levels = c("None", "Primary", "Secondary", "Higher"))) %>%
  mutate(school_level = recode(school_level, "None" = "< Secondary", "Primary" = "< Secondary",
                               "Secondary" = "≥ Secondary", "Higher" = "≥ Secondary"))

# split into separate dataframes for each village
hookworm_df_man <- hookworm_df %>%
  filter(village == "Mandena")
hookworm_df_sara <- hookworm_df %>%
  filter(village == "Sarahandrano")
hookworm_df_andat <- hookworm_df %>%
  filter(village == "Andatsakala")

# filter out isolated clusters for sarahandrano and andatsakala
hookworm_df_sara <- hookworm_df_sara %>%
  filter(cluster_assignment != 4)
hookworm_df_andat <- hookworm_df_andat %>%
  filter(cluster_assignment != 4)

###########
## Modeling
###########

# change cluster assignment to factor
hookworm_df_man <- hookworm_df_man %>%
  mutate(cluster_assignment = as.factor(cluster_assignment))
hookworm_df_sara <- hookworm_df_sara %>%
  mutate(cluster_assignment = as.factor(cluster_assignment))
hookworm_df_andat <- hookworm_df_andat %>%
  mutate(cluster_assignment = as.factor(cluster_assignment))

man_human <- glm(human_hookworm ~
                    age +
                    gender +
                    school_level +
                    cluster_assignment,
                  data = hookworm_df_man,
                  family = "binomial",
                  na.action = na.fail)
summary(man_human)

m1_plot <- jtools::effect_plot(man_human, cluster_assignment,
                               x.label = "Cluster", y.label = "Predicted Probability of Infection",
                               colors = "maroon")

m1_plot <- m1_plot +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

man_dog <- glm(dog_hookworm ~
                    age +
                    gender +
                    school_level +
                    pet_dogs +
                    cluster_assignment,
                  data = hookworm_df_man,
                  family = "binomial",
                  na.action = na.fail)
summary(man_dog)

m2_plot <- jtools::effect_plot(man_dog, cluster_assignment,
                               x.label = "Cluster", y.label = "Predicted Probability of Infection",
                               colors = "maroon")

m2_plot <- m2_plot +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

sara_human <- glm(human_hookworm ~
                   age +
                   gender +
                   school_level +
                   cluster_assignment,
                 data = hookworm_df_sara,
                 family = "binomial",
                 na.action = na.fail)
summary(sara_human)

m3_plot <- jtools::effect_plot(sara_human, cluster_assignment,
                               x.label = "Cluster", y.label = "Predicted Probability of Infection",
                               colors = "darkorange")

m3_plot <- m3_plot +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

sara_dog <- glm(dog_hookworm ~
                 age +
                 gender +
                 school_level +
                 pet_dogs +
                 cluster_assignment,
               data = hookworm_df_sara,
               family = "binomial",
               na.action = na.fail)
summary(sara_dog)

m4_plot <- jtools::effect_plot(sara_dog, cluster_assignment,
                               x.label = "Cluster", y.label = "Predicted Probability of Infection",
                               colors = "darkorange")

m4_plot <- m4_plot +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

andat_human <- glm(human_hookworm ~
                    age +
                    gender +
                    school_level +
                    cluster_assignment,
                  data = hookworm_df_andat,
                  family = "binomial",
                  na.action = na.fail)
summary(andat_human)

m5_plot <- jtools::effect_plot(andat_human, cluster_assignment,
                               x.label = "Cluster", y.label = "Predicted Probability of Infection",
                               colors = "navy")

m5_plot <- m5_plot +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

andat_dog <- glm(dog_hookworm ~
                  age +
                  gender +
                  school_level +
                  pet_dogs +
                  cluster_assignment,
                data = hookworm_df_andat,
                family = "binomial",
                na.action = na.fail)
summary(andat_dog)

m6_plot <- jtools::effect_plot(andat_dog, cluster_assignment,
                               x.label = "Cluster", y.label = "Predicted Probability of Infection",
                               colors = "navy")

m6_plot <- m6_plot +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

# combine model plots
model_plots <- gridExtra::grid.arrange(grobs = list(m1_plot, m3_plot, m5_plot,
                                                    m2_plot, m4_plot, m6_plot),
                                              ncol = 3, nrow = 2)
ggsave("model_plots.pdf", model_plots, width = 15, height = 11)


## EXTRA CODE ##
# 
# # compute prevalence
# # overall
# hum_hook_prev <- prevalence(hookworm_df_sara, "human_hookworm")
# dog_hook_prev <- prevalence(hookworm_df_sara, "dog_hookworm")
# 
# p4 <- ggplot(hum_hook_prev, aes(x = as.factor(human_hookworm), y = prevalence,
#                           fill = as.factor(human_hookworm),
#                           label = prevalence)) +
#   geom_col() +
#   geom_text(aes(label = scales::label_number(suffix = "%", accuracy = 1)(prevalence)),
#             hjust = -0.5, size = 5) +
#   scale_x_discrete(labels = c("Not Infected", "Infected")) +
#   scale_fill_manual(values = c('1' = "maroon", '0' = "lightblue")) +
#   scale_y_continuous(limits = c(0, 100)) +
#   xlab("Infection Status") +
#   ylab("Percent") +
#   labs(title = "Human Hookworm") +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position = "none") +
#   theme(axis.text.y = element_text(size = 24, color = "black")) +
#   theme(axis.text.x = element_text(size = 24, color = "black")) +
#   theme(axis.title.y = element_text(size = 0)) +
#   theme(axis.title.x = element_text(size = 24)) +
#   theme(plot.title = element_text(hjust = 0.5, size = 24))
# 
# ggsave("hum_hook_prev.png", p4)
# 
# p5 <- ggplot(dog_hook_prev, aes(x = as.factor(dog_hookworm), y = prevalence,
#                           fill = as.factor(dog_hookworm),
#                           label = prevalence)) +
#   geom_col() +
#   geom_text(aes(label = scales::label_number(suffix = "%", accuracy = 1)(prevalence)),
#             hjust = -0.5, size = 5) +
#   scale_x_discrete(labels = c("Not Infected", "Infected")) +
#   scale_fill_manual(values = c('1' = "maroon", '0' = "lightblue")) +
#   scale_y_continuous(limits = c(0, 100)) +
#   xlab("Infection Status") +
#   ylab("Percent") +
#   labs(title = "Canine Hookworm") +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position = "none") +
#   theme(axis.text.y = element_text(size = 24, color = "black")) +
#   theme(axis.text.x = element_text(size = 24, color = "black")) +
#   theme(axis.title.y = element_text(size = 0)) +
#   theme(axis.title.x = element_text(size = 24)) +
#   theme(plot.title = element_text(hjust = 0.5, size = 24))
# 
# ggsave("dog_hook_prev.png", p5)
# 
# # by village
# # create vector of villages
# villages <- c(unique(hookworm_df$village))
# # iterate prevalence function over villages and subtypes
# for (i in villages) {
#   result <- prevalence(hookworm_df, "human_hookworm", "village", i)
# }
# for (i in villages) {
#   result <- prevalence(hookworm_df, "dog_hookworm", "village", i)
# }
# 
# # extract cluster assignments
# net_data$mandena$nl <- net_data$mandena$nl %>%
#   mutate(cluster_assignment = role_analysis_list$mandena$cluster_assignments$best_fit)
# net_data$sarahandrano$nl <- net_data$sarahandrano$nl %>%
#   mutate(cluster_assignment = role_analysis_list$sarahandrano$cluster_assignments$best_fit)
# net_data$andatsakala$nl <- net_data$andatsakala$nl %>%
#   mutate(cluster_assignment = role_analysis_list$andatsakala$cluster_assignments$best_fit)
# 
# # join into one dataframe
# cluster_assignments <- net_data$mandena$nl %>%
#   bind_rows(net_data$sarahandrano$nl, net_data$andatsakala$nl) %>%
#   select(social_netid, cluster_assignment)
# 
# # add cluster assignments to hookworm dataframe
# hookworm_df <- hookworm_df %>%
#   left_join(cluster_assignments, by = "social_netid")
# 
# animal_df <- read_csv("/Users/tylerbarrett/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/Survey_Animal_Interaction.csv")
# 
# animal_df <- animal_df %>%
#   select(social_netid, pet_dogs)
# 
# hookworm_df <- hookworm_df %>%
#   left_join(animal_df)
# 
# # filter to complete cases for modeling
# hookworm_df <- hookworm_df %>%
#   filter(!is.na(age) & !is.na(gender) & !is.na(school_level), !is.na(pet_dogs))
# 
# # standardize numeric variables
# hookworm_df <- hookworm_df %>%
#   mutate(age = scale(age))
# 
# # relevel education for models
# hookworm_df <- hookworm_df %>%
#   mutate(school_level = factor(school_level, levels = c("None", "Primary", "Secondary", "Higher"))) %>%
#   mutate(school_level = recode(school_level, "None" = "< Secondary", "Primary" = "< Secondary",
#                                "Secondary" = "≥ Secondary", "Higher" = "≥ Secondary"))
# 
# # split into separate dataframes for each village
# hookworm_df_man <- hookworm_df %>%
#   filter(village == "Mandena")
# hookworm_df_sara <- hookworm_df %>%
#   filter(village == "Sarahandrano")
# hookworm_df_andat <- hookworm_df %>%
#   filter(village == "Andatsakala")
# 
# table1 <- hookworm_df_man %>%
#   gtsummary::tbl_summary(include = c(dog_hookworm),
#               by = cluster_assignment, missing = "ifany")
# table2 <- hookworm_df_sara %>%
#   gtsummary::tbl_summary(include = c(dog_hookworm, gender, school_level),
#                          by = cluster_assignment, missing = "ifany")
# table3 <- hookworm_df_andat %>%
#   gtsummary::tbl_summary(include = c(dog_hookworm),
#                          by = cluster_assignment, missing = "ifany")
# 
# table4 <- gtsummary::tbl_merge(tbls = list(table1, table2, table3))
# 
# ###########
# ## Modeling
# ###########
# 
# # change cluster assignment to factor
# hookworm_df_man <- hookworm_df_man %>%
#   mutate(cluster_assignment = as.factor(cluster_assignment))
# hookworm_df_sara <- hookworm_df_sara %>%
#   mutate(cluster_assignment = as.factor(cluster_assignment))
# hookworm_df_andat <- hookworm_df_andat %>%
#   mutate(cluster_assignment = as.factor(cluster_assignment))
# 
# # drop cluster 4 from sara with one participant
# hookworm_df_sara <- hookworm_df_sara %>%
#   filter(cluster_assignment != 4)
# 
# # build global models
# 
# man_global <- glm(dog_hookworm ~
#                     age +
#                     gender +
#                     school_level +
#                     cluster_assignment,
#                   data = hookworm_df_man,
#                   family = "binomial",
#                   na.action = na.fail)
# summary(man_global)
# 
# hookworm_df_sara <- hookworm_df_sara %>%
#   mutate(cluster_assignment = relevel(cluster_assignment, ref = "2"))
# 
# sara_global <- glm(dog_hookworm ~
#                     age +
#                     gender +
#                     school_level +
#                     pet_dogs +
#                     cluster_assignment,
#                     #pet_dogs*cluster_assignment,
#                   data = hookworm_df_sara,
#                   family = "binomial",
#                   na.action = na.fail)
# summary(sara_global)
# model_output <- summary(sara_global)
# model_output <- as.data.frame(model_output$coefficients)
# model_output <- rownames_to_column(model_output, var = "Term")
# performance::check_model(sara_global)
# 
# # Calculate Odds Ratios and 95% Confidence Intervals
# model_output$OddsRatio <- exp(model_output$Estimate)
# model_output$LowerCI <- exp(model_output$Estimate - 1.96 * model_output$`Std. Error`)
# model_output$UpperCI <- exp(model_output$Estimate + 1.96 * model_output$`Std. Error`)
# 
# # Prepare data for plotting
# plot_data <- model_output %>%
#   select(Term, OddsRatio, LowerCI, UpperCI) %>%
#   mutate(Term = factor(Term, levels = Term)) %>%
#   filter(Term != "(Intercept)")
# 
# # Plotting the Forest Plot
# ggplot(plot_data, aes(x = Term, y = OddsRatio, ymin = LowerCI, ymax = UpperCI)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
#   coord_flip() +
#   theme_minimal() +
#   xlab("Variable") +
#   ylab("Odds Ratio (95% CI)") +
#   ggtitle("Forest Plot of Model Estimates")
# 
# plot_data <- plot_data %>%
#   mutate(Term = recode(Term,
#                               "genderMale" = "Gender (Men vs. Women)",
#                               "school_level≥ Secondary" = "Education (≥ Secondary vs. Less)",
#                               "pet_dogsYes" = "Own Dogs (Yes vs. No)",
#                               "cluster_assignment1" = "Role 1 (vs. Role 2)",
#                               "cluster_assignment3" = "Role 3 (vs. Role 2)",
#                               "age" = "Age"))
# 
# p6 <- ggplot(plot_data, aes(x = fct_rev(Term), y = OddsRatio, ymin = LowerCI, ymax = UpperCI)) +
#   geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
#   geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI), size = 2, width = 0.2) +
#   geom_point(aes(x = fct_rev(Term)), size = 8, color = "purple", alpha = 100) +
#   geom_point(shape = 1,size = 8, stroke = 1.5, color = "black") +
#   coord_flip() +  # flip coordinates (puts labels on y axis)
#   xlab("") + ylab("Odds Ratio (95% Confidince Interval)") + 
#   scale_x_discrete(limits = c("Role 3 (vs. Role 2)", "Role 1 (vs. Role 2)", "Own Dogs (Yes vs. No)",
#                               "Education (≥ Secondary vs. Less)", "Gender (Men vs. Women)", "Age")) +
#   theme_classic() +
#   theme(axis.text.y = element_text(size = 24, color = "black")) +
#   theme(axis.text.x = element_text(size = 24, color = "black")) +
#   theme(axis.title.y = element_text(size = 24)) +
#   theme(axis.title.x = element_text(size = 24)) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave("dog_coef_plot.png", p6, width = 13)
# 
# 
# # use dredge for model comparison
# sara_global_dredge <- dredge(sara_global)
# 
# # average models with delta AICc < 2
# sara_global_avg <- model.avg(sara_global_dredge, subset = delta < 10)
# summary(sara_global_avg)
# plot(sara_global_avg)
# 
# andat_global <- glm(dog_hookworm ~
#                      age +
#                      gender +
#                      school_level +
#                      cluster_assignment,
#                    data = hookworm_df_andat,
#                    family = "binomial",
#                    na.action = na.fail)
# summary(andat_global)
# 
# man_global <- glm(human_hookworm ~
#                     age +
#                     gender +
#                     school_level +
#                     cluster_assignment,
#                   data = hookworm_df_man,
#                   family = "binomial",
#                   na.action = na.fail)
# summary(man_global)
# 
# sara_global <- glm(human_hookworm ~
#                      age +
#                      gender +
#                      school_level +
#                      cluster_assignment,
#                    data = hookworm_df_sara,
#                    family = "binomial",
#                    na.action = na.fail)
# summary(sara_global)
# 
# andat_global <- glm(human_hookworm ~
#                       age +
#                       gender +
#                       school_level +
#                       cluster_assignment,
#                     data = hookworm_df_andat,
#                     family = "binomial",
#                     na.action = na.fail)
# summary(andat_global)


