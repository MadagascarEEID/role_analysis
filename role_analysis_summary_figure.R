# Role Analysis Summary Figure
# Created by Tyler Barrett on September 30, 2025
# This code takes the results of the role_analysis.r script and generates
# a summary figure to describe the role structure at a high level.

#################
#   Preamble    #
#################

# Load Packages
  library(tidyverse)
  library(tidygraph)
  library(ggraph)
  library(patchwork)
  library(scales)
  library(RColorBrewer)

# Set Global Font and Text Size
  font_family <- if (Sys.info()[["sysname"]] == "Windows") "sans" else "Helvetica"
  base_pt     <- 35
  legend_pt   <- 35
  txt_mm      <- base_pt / .pt
  legend_txt_mm <- legend_pt / .pt
  
  theme_set(
    theme_get() +
      theme(text = element_text(family = font_family, size = base_pt))
  )

# Functions
  # Create Summary Dataframe  
    create_combined_summary_df <- function(clustering_variables, cluster_groups, village_name) {
      
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
        mutate(
          relation = case_when(
            str_detect(variable, "^std_freetime") ~ "Free Time",
            str_detect(variable, "^std_food_help_provided") ~ "Food Help Provided",
            str_detect(variable, "^std_food_help_received") ~ "Food Help Received",
            str_detect(variable, "^std_farm_help_provided") ~ "Farm Help Provided",
            str_detect(variable, "^std_farm_help_received") ~ "Farm Help Received"
          ),
          triad_position = str_remove(variable, "^(std_freetime_|std_food_help_provided_|std_food_help_received_|std_farm_help_provided_|std_farm_help_received_)"),
          measure_type = "Triad Position",
          measure = triad_position,
          village = village_name
        ) %>%
        select(village, group, relation, measure_type, measure, value)
      
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
        mutate(
          relation = case_when(
            str_detect(variable, "^std_freetime") ~ "Free Time",
            str_detect(variable, "^std_food_help_provided") ~ "Food Help Provided",
            str_detect(variable, "^std_food_help_received") ~ "Food Help Received",
            str_detect(variable, "^std_farm_help_provided") ~ "Farm Help Provided",
            str_detect(variable, "^std_farm_help_received") ~ "Farm Help Received"
          ),
          centrality_measure = str_remove(variable, "^(std_freetime_|std_food_help_provided_|std_food_help_received_|std_farm_help_provided_|std_farm_help_received_)") %>%
            str_replace_all("_", " ") %>% 
            str_replace("betweenness scores", "Betweenness") %>%
            str_to_title(),
          measure_type = "Centrality",
          measure = centrality_measure,
          village = village_name
        ) %>%
        select(village, group, relation, measure_type, measure, value)
      
      # Combine both dataframes
      combined_df <- bind_rows(triad_summary_df, cent_summary_df)
      
      return(combined_df)
  }
    
  #	Process Data
    process_village_data <- function(village_name, data) {
      #	"""
      #	Args:
      #		village_name: string name of village to filter
      #		data: data frame containing village network measures
      #	Returns:
      #		list with triad and centrality tibbles
      #	Notes:
      #		Filters data by village, categorizes triads by pattern,
      #		and aggregates both triad and centrality measures.
      #	"""
      
      #	Filter village data
      village_data <- data %>% filter(village == village_name)
      
      #	Define triad categories
      reciprocal_triads <- c("201_B", "210_B", "300")
      sender_triads     <- c("012_S", "021C_S", "021U_S", "111D_S",
                             "021D_S", "030T_S", "120D_S")
      receiver_triads   <- c("012_E", "021C_E", "021D_E", "111U_E",
                             "021U_E", "030T_E", "120U_E")
      bridge_triads     <- c("021C_B", "030C", "030T_B", "120C_B")
      
      #	Process triad data
      triad_data <- village_data %>%
        filter(measure_type == "Triad Position") %>%
        group_by(group, measure) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          pattern = case_when(
            measure %in% reciprocal_triads ~ "Reciprocal",
            measure %in% sender_triads     ~ "Unreciprocated\nOut",
            measure %in% receiver_triads   ~ "Unreciprocated\nIn",
            measure %in% bridge_triads     ~ "Bridging",
            TRUE                           ~ "Other"
          )
        )
      
      #	Process centrality data
      centrality_data <- village_data %>%
        filter(measure_type == "Centrality") %>%
        group_by(group, measure) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
      
      #	Assemble result
      return(list(triad = triad_data, centrality = centrality_data))
  }
    
  # Utilities For Processing Matrices
    #	Sanitize Square Matrix
      sanitize_square <- function(m) {
        #	"""
        #	Args:
        #		m: numeric matrix
        #	Returns:
        #		sanitized matrix with NA->0, negatives->0, diagonal->0
        #	Notes:
        #		Ensures adjacency matrix has valid values for graph construction.
        #	"""
        
        #	Clean values
        m[is.na(m)] <- 0
        m[m < 0]    <- 0
        diag(m)     <- 0
        
        return(m)
  }
      
    #	Create Edge List from Matrix
      make_edges <- function(m, label) {
        #	"""
        #	Args:
        #		m: adjacency matrix (row->col = directed edge)
        #		label: string label for edge type
        #	Returns:
        #		tibble with columns: from, to, type
        #	Notes:
        #		Extracts all positive entries as directed edges.
        #	"""
        
        #	Sanitize input
        m <- sanitize_square(m)
        
        #	Extract edge indices
        idx <- which(m > 0, arr.ind = TRUE)
        
        #	Early return if no edges
        if (nrow(idx) == 0) {
          return(tibble(from = integer(), to = integer(), type = character()))
        }
        
        #	Build edge table
        tibble(from = idx[, "row"], to = idx[, "col"], type = label)
  }
      
    #	Build Village Graph
      build_village_graph <- function(fhp, fhr, fohp, fohr, ft, node_tbl) {
        #	"""
        #	Args:
        #		fhp, fhr: farm helping matrices (planting, reaping)
        #		fohp, fohr: food exchange matrices (planting, reaping)
        #		ft: free time matrix
        #		node_tbl: tibble with node attributes (id, n, group, shape)
        #	Returns:
        #		tbl_graph object with directed edges and sized nodes
        #	Notes:
        #		Averages paired matrices, combines edge types, adds size mapping.
        #	"""
        
        #	Average paired matrices
        food_avg <- sanitize_square((sanitize_square(fohp) + sanitize_square(fohr)) / 2)
        farm_avg <- sanitize_square((sanitize_square(fhp)  + sanitize_square(fhr))  / 2)
        ft       <- sanitize_square(ft)
        
        #	Create edge lists
        edges <- bind_rows(
          make_edges(food_avg, "Food Exchange"),
          make_edges(farm_avg, "Co-Farmworking"),
          make_edges(ft,       "Free Time")
        )
        
        #	Prepare node table
        nodes <- node_tbl %>%
          mutate(
            id = as.integer(id),
            size_mapped = .map_node_size(n)
          )
        
        #	Build graph
        tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
  }
    
  #	Normalize group labels
    normalize_groups <- function(x) {
      #	"""
      #	Args:
      #		x: character vector of group labels
      #	Returns:
      #		character vector with "Popular" -> "Most Popular"
      #	"""
      
      x_chr <- as.character(x)
      ifelse(x_chr == "Popular", "Most Popular", x_chr)
  }
    
  #	Normalize measure labels
    normalize_measure <- function(x) {
      #	"""
      #	Args:
      #		x: character vector of measure names
      #	Returns:
      #		character vector with standardized closeness labels
      #	"""
      
      x_chr <- trimws(as.character(x))
      recode(x_chr,
             "Closeness"     = "Closeness Out",
             "ClosenessOut"  = "Closeness Out",
             "Out Closeness" = "Closeness Out",
             .default = x_chr)
  }

#################################################
#   Process Role Analysis Results For Figure    #
#################################################

# Create Dataframe For Each Village
  man_summary_df <- create_combined_summary_df(
    role_analysis_list$mandena$clustering_variables, 
    cluster_groups_man, 
    "Mandena"
  )
  
  sara_summary_df <- create_combined_summary_df(
    role_analysis_list$sarahandrano$clustering_variables, 
    cluster_groups_sara, 
    "Sarahandrano"
  )
  
  andat_summary_df <- create_combined_summary_df(
    role_analysis_list$andatsakala$clustering_variables, 
    cluster_groups_andat, 
    "Andatsakala"
  )
  
# Combine All Villages Into One Dataframe
  all_villages_summary_df <- bind_rows(man_summary_df, sara_summary_df, andat_summary_df)

#########################################################
#   Hardcode Role Adjacency Matrices For Blockmodels    #  
#########################################################
  
#	Village A Matrices (8x8)
  A_fohp <- matrix(c(
    0,0,1,0,1,1,1,1,
    0,0,0,1,0,0,1,1,
    0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,
    1,1,0,0,0,0,1,1,
    1,0,0,0,0,0,1,1,
    1,1,0,0,1,1,0,0,
    1,1,0,0,1,1,1,0
  ), 8, 8, byrow = TRUE)
  rownames(A_fohp) <- colnames(A_fohp) <- as.character(1:8)
  
  A_fohr <- matrix(0, 8, 8)
  rownames(A_fohr) <- colnames(A_fohr) <- as.character(1:8)
  
  A_fhp <- matrix(c(
    0,1,0,0,0,0,1,1,
    0,0,0,1,0,1,1,0,
    0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,
    0,1,0,0,0,1,1,1,
    0,1,0,0,0,0,1,1,
    0,1,0,1,1,1,0,1,
    0,1,0,0,1,1,1,0
  ), 8, 8, byrow = TRUE)
  rownames(A_fhp) <- colnames(A_fhp) <- as.character(1:8)
  
  A_fhr <- matrix(0, 8, 8)
  rownames(A_fhr) <- colnames(A_fhr) <- as.character(1:8)
  
  A_ft <- matrix(c(
    0,0,1,0,0,0,1,0,
    0,0,1,1,0,0,1,0,
    0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,1,1,
    0,0,0,0,1,0,1,1,
    1,1,0,0,1,0,0,0,
    1,0,0,0,0,1,1,0
  ), 8, 8, byrow = TRUE)
  rownames(A_ft) <- colnames(A_ft) <- as.character(1:8)

#	Village B Matrices (5x5)
  B_fhp <- matrix(c(
    0,1,0,1,0,
    1,0,1,1,0,
    0,0,0,0,0,
    1,1,0,0,0,
    0,0,0,0,0
  ), 5, 5, byrow = TRUE)
  rownames(B_fhp) <- colnames(B_fhp) <- as.character(1:5)
  
  B_fhr <- matrix(0, 5, 5)
  rownames(B_fhr) <- colnames(B_fhr) <- as.character(1:5)
  
  B_fohp <- matrix(c(
    0,1,0,1,0,
    0,0,0,1,0,
    0,1,0,0,0,
    1,1,0,0,0,
    0,0,0,0,0
  ), 5, 5, byrow = TRUE)
  rownames(B_fohp) <- colnames(B_fohp) <- as.character(1:5)
  
  B_fohr <- matrix(0, 5, 5)
  rownames(B_fohr) <- colnames(B_fohr) <- as.character(1:5)
  
  B_ft <- matrix(c(
    0,1,0,1,0,
    0,0,0,1,0,
    0,1,0,0,0,
    0,1,0,0,0,
    0,0,0,0,0
  ), 5, 5, byrow = TRUE)
  rownames(B_ft) <- colnames(B_ft) <- as.character(1:5)

#	Village C Matrices (4x4)
  C_fhp <- matrix(c(
    0,0,0,0,
    1,0,1,0,
    0,1,0,0,
    0,0,0,0
  ), 4, 4, byrow = TRUE)
  rownames(C_fhp) <- colnames(C_fhp) <- as.character(1:4)
  
  C_fhr <- matrix(0, 4, 4)
  rownames(C_fhr) <- colnames(C_fhr) <- as.character(1:4)
  
  C_fohp <- matrix(c(
    0,0,0,0,
    0,0,1,0,
    0,1,0,0,
    0,0,0,0
  ), 4, 4, byrow = TRUE)
  rownames(C_fohp) <- colnames(C_fohp) <- as.character(1:4)
  
  C_fohr <- matrix(0, 4, 4)
  rownames(C_fohr) <- colnames(C_fohr) <- as.character(1:4)
  
  C_ft <- matrix(c(
    0,0,0,0,
    0,0,1,0,
    0,1,0,0,
    0,0,0,0
  ), 4, 4, byrow = TRUE)
  rownames(C_ft) <- colnames(C_ft) <- as.character(1:4)

###########################
#   Add Node Attributes   #
###########################
  
#	Node Tables With Sample Sizes
  nodes_A <- tibble(
    id    = 1:8,
    n     = c(27, 7, 41, 60, 35, 27, 54, 11),
    group = c("Core","Core","Periphery","Periphery","Core","Core","Popular","Core"),
    shape = c("square","square","circle","circle","square","square","triangle","square")
  )
  
  nodes_B <- tibble(
    id    = 1:5,
    n     = c(137, 168, 99, 28, 3),
    group = c("Core","Popular","Periphery","Core","Periphery"),
    shape = c("square","triangle","circle","square","circle")
  )
  
  nodes_C <- tibble(
    id    = 1:4,
    n     = c(230, 159, 210, 1),
    group = c("Periphery","Popular","Core","Periphery"),
    shape = c("circle","triangle","square","circle")
  )

#	Global Node Size Mapping
  .all_n <- c(nodes_A$n, nodes_B$n, nodes_C$n)
  .map_node_size <- function(n, to = c(5, 14), pow = 0.8) {
  #	"""
  #	Args:
  #		n: numeric vector of sample sizes
  #		to: target range for rescaling (default c(5, 14))
  #		pow: power transformation (default 0.8)
  #	Returns:
  #		numeric vector of mapped sizes
  #	Notes:
  #		Applies power transform then rescales to target range
  #		based on global min/max from all villages.
  #	"""
  
  #	Transform values
  v_all <- .all_n^pow
  v     <- n^pow
  
  #	Rescale to target range
  scales::rescale(v, to = to, from = range(v_all, na.rm = TRUE))
}

#################################
#   Generate Network Figures    #
#################################

#	Edge colors
  dark2 <- brewer.pal(8, "Dark2")
  edge_cols <- c(
    "Food Exchange"  = dark2[1],
    "Co-Farmworking" = dark2[2],
    "Free Time"      = dark2[3]
  )

#	Plot Network with Algorithmic Layout
  plot_network_algorithmic <- function(graph, title,
                                       layout_algo   = "fr",
                                       seed          = 42,
                                       niter         = 1200,
                                       fan_strength  = 0.75,
                                       drop_isolates = TRUE,
                                       cap_quantile  = 0.20,
                                       cap_fudge     = 0.25) {
    #	"""
    #	Args:
    #		graph: tbl_graph object
    #		title: plot title string
    #		layout_algo: layout algorithm ("fr" or "kk")
    #		seed: random seed for layout
    #		niter: number of layout iterations
    #		fan_strength: curve strength for parallel edges
    #		drop_isolates: whether to remove isolated nodes
    #		cap_quantile: quantile for arrow cap distance
    #		cap_fudge: additional cap padding (mm)
    #	Returns:
    #		ggplot object
    #	Notes:
    #		Uses force-directed layout with adaptive arrow positioning.
    #	"""
    
    #	Filter isolates if requested
    g <- graph
    if (drop_isolates) {
      g <- g %>% activate(nodes) %>% filter(!node_is_isolated())
    }
    
    #	Compute arrow cap distance
    sizes <- g %>% activate(nodes) %>% as_tibble() %>% pull(size_mapped)
    sizes <- sizes[is.finite(sizes)]
    if (!length(sizes)) sizes <- 6
    
    diam_ref <- as.numeric(quantile(sizes, probs = cap_quantile, names = FALSE, type = 7))
    cap_mm   <- (diam_ref / 2) + cap_fudge
    cap_mm   <- max(cap_mm, 1.6)
    start_mm <- max(cap_mm * 0.65, 1)
    
    #	Create plot
    set.seed(seed)
    ggraph(g, layout = layout_algo, niter = niter) +
      geom_edge_fan(
        aes(color = type),
        show.legend = FALSE,
        arrow      = arrow(length = unit(2.4, "mm"), type = "closed"),
        start_cap  = circle(start_mm, "mm"),
        end_cap    = circle(cap_mm,  "mm"),
        lineend    = "round",
        width      = 1.05,
        alpha      = 0.70,
        strength   = fan_strength
      ) +
      geom_node_point(
        aes(shape = shape, size = size_mapped),
        show.legend = FALSE,
        fill = "black", color = "black", stroke = 0.55
      ) +
      scale_size_identity() +
      scale_shape_manual(values = c(triangle = 24, square = 22, circle = 21)) +
      scale_edge_color_manual(values = edge_cols) +
      labs(title = title) +
      theme_void() +
      theme(
        plot.title   = element_text(size = base_pt, face = "bold", hjust = 0),
        plot.margin  = margin(6, 6, 6, 6),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6)
      )
}

#	Construct village graphs
  g_a <- build_village_graph(A_fhp, A_fhr, A_fohp, A_fohr, A_ft, nodes_A)
  g_b <- build_village_graph(B_fhp, B_fhr, B_fohp, B_fohr, B_ft, nodes_B)
  g_c <- build_village_graph(C_fhp, C_fhr, C_fohp, C_fohr, C_ft, nodes_C)

#	Generate network plots
  layout_algo <- "fr"
  pA <- plot_network_algorithmic(g_a, "A. Village A Blockmodel", layout_algo = layout_algo)
  pB <- plot_network_algorithmic(g_b, "Village B Blockmodel",    layout_algo = layout_algo)
  pC <- plot_network_algorithmic(g_c, "Village C Blockmodel",    layout_algo = layout_algo)

# Build Legend
  #	Size legend ticks
    size_ticks <- sort(unique(round(quantile(.all_n, probs = c(0, 0.5, 1)))))
    size_tick_sizes <- .map_node_size(size_ticks)
  
  #	Legend panel
    p_legend <- ggplot() +
      # Role category
        annotate("text", x = 0.5, y = 4.9, label = "Role Category",
                 size = legend_txt_mm, fontface = "bold", hjust = 0, family = font_family) +
        annotate("point", x = 0.8, y = 4.45, shape = 24, size = 6.0,
                 fill = "black", color = "black", stroke = 0.4) +
        annotate("text",  x = 1.08, y = 4.45, label = "Popular (triangle)",
                 size = legend_txt_mm, fontface = "bold", hjust = 0, family = font_family) +
        annotate("point", x = 0.8, y = 3.95, shape = 22, size = 6.0,
                 fill = "black", color = "black", stroke = 0.4) +
        annotate("text",  x = 1.08, y = 3.95, label = "Core (square)",
                 size = legend_txt_mm, fontface = "bold", hjust = 0, family = font_family) +
        annotate("point", x = 0.8, y = 3.45, shape = 21, size = 6.0,
                 fill = "black", color = "black", stroke = 0.4) +
        annotate("text",  x = 1.08, y = 3.45, label = "Periphery (circle)",
                 size = legend_txt_mm, fontface = "bold", hjust = 0, family = font_family) +
      
      # Sample size scale
        annotate("text", x = 0.5, y = 2.85, label = "Number of People",
                 size = legend_txt_mm, fontface = "bold", hjust = 0, family = font_family) +
        annotate("point", x = 0.8, y = 2.55, shape = 21, size = size_tick_sizes[1],
                 fill = "black", color = "black", stroke = 0.4) +
        annotate("text",  x = 1.08, y = 2.55, label = paste0(size_ticks[1]),
                 size = legend_txt_mm, hjust = 0, family = font_family, fontface = "bold") +
        annotate("point", x = 0.8, y = 2.20, shape = 21, size = size_tick_sizes[2],
                 fill = "black", color = "black", stroke = 0.4) +
        annotate("text",  x = 1.08, y = 2.20, label = paste0(size_ticks[2]),
                 size = legend_txt_mm, hjust = 0, family = font_family, fontface = "bold") +
        annotate("point", x = 0.8, y = 1.85, shape = 21, size = size_tick_sizes[3],
                 fill = "black", color = "black", stroke = 0.4) +
        annotate("text",  x = 1.08, y = 1.85, label = paste0(size_ticks[3]),
                 size = legend_txt_mm, hjust = 0, family = font_family, fontface = "bold") +
      
      # Edge types
        annotate("text", x = 0.5, y = 1.30, label = "Relations",
                 size = legend_txt_mm, fontface = "bold", hjust = 0, family = font_family) +
        annotate("segment", x = 0.7, xend = 1.05, y = 1.05, yend = 1.05,
                 colour = edge_cols["Food Exchange"], size = 1.2,
                 arrow = arrow(length = unit(2, "mm"), type = "closed")) +
        annotate("text", x = 1.1, y = 1.05, label = "Food Exchange",
                 size = legend_txt_mm, hjust = 0, family = font_family, fontface = "bold") +
        annotate("segment", x = 0.7, xend = 1.05, y = 0.80, yend = 0.80,
                 colour = edge_cols["Co-Farmworking"], size = 1.2,
                 arrow = arrow(length = unit(2, "mm"), type = "closed")) +
        annotate("text", x = 1.1, y = 0.80, label = "Co-Farmworking",
                 size = legend_txt_mm, hjust = 0, family = font_family, fontface = "bold") +
        annotate("segment", x = 0.7, xend = 1.05, y = 0.55, yend = 0.55,
                 colour = edge_cols["Free Time"], size = 1.2,
                 arrow = arrow(length = unit(2, "mm"), type = "closed")) +
        annotate("text", x = 1.1, y = 0.55, label = "Free Time",
                 size = legend_txt_mm, hjust = 0, family = font_family, fontface = "bold") +
        
        xlim(0.4, 2.0) + ylim(0.45, 5.05) +
        theme_void() +
        theme(plot.margin = margin(6, 8, 6, 8))

#######################################
#   Triad And Centrality Summaries    #
#######################################
  
#	Extract village-level summaries
  mandena_data <- process_village_data("Mandena",      all_villages_summary_df)
  sara_data    <- process_village_data("Sarahandrano", all_villages_summary_df)
  andat_data   <- process_village_data("Andatsakala",  all_villages_summary_df)

#	Combine triad data
  all_villages_triad <- bind_rows(
    mandena_data$triad %>% mutate(village = "Mandena"),
    sara_data$triad    %>% mutate(village = "Sarahandrano"),
    andat_data$triad   %>% mutate(village = "Andatsakala")
  ) %>%
    mutate(group = normalize_groups(group))

#	Combine centrality data
  all_villages_centrality <- bind_rows(
    mandena_data$centrality %>% mutate(village = "Mandena"),
    sara_data$centrality    %>% mutate(village = "Sarahandrano"),
    andat_data$centrality   %>% mutate(village = "Andatsakala")
  ) %>%
    mutate(
      group   = normalize_groups(group),
      measure = normalize_measure(measure)
    )

#	Aggregate Triad Patterns
  triad_matrix <- all_villages_triad %>%
    filter(pattern != "Other") %>%
    group_by(group, pattern) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      n_villages = n_distinct(village),
      .groups = "drop"
    )

#	Create Triad Plot
  p_triad <- ggplot(triad_matrix,
                    aes(x = pattern,
                        y = factor(group, levels = c("Periphery","Core","Most Popular")),
                        fill = mean_value)) +
    geom_tile(color = "black", size = 1.2) +
    geom_text(aes(label = sprintf("%.2f", mean_value)),
              size = txt_mm, color = "black", fontface = "bold", family = font_family) +
    scale_fill_gradient2(low = "#313695", mid = "#ffffbf", high = "#a50026",
                         midpoint = 0, limits = c(-1.5, 1.5), name = "Z-score") +
    scale_y_discrete(labels = c("Periphery","Core","Popular")) +
    labs(title = "B. Triad Position Frequencies (Standardized)", x = NULL, y = NULL) +
    theme_minimal(base_family = font_family, base_size = base_pt) +
    guides(
      fill = guide_colourbar(
        direction      = "vertical",
        barheight      = unit(260, "pt"),
        barwidth       = unit(26,  "pt"),
        title.position = "top",
        ticks.colour   = "black"
      )
    ) +
    theme(
      plot.title        = element_text(size = base_pt, face = "bold", hjust = 0, colour = "black"),
      axis.text.x       = element_text(angle = 0, hjust = 0.5, size = base_pt, face = "bold", colour = "black"),
      axis.text.y       = element_text(size = base_pt, face = "bold", colour = "black"),
      legend.position   = "right",
      legend.title      = element_text(size = legend_pt, face = "bold", colour = "black"),
      legend.text       = element_text(size = legend_pt, face = "bold", colour = "black"),
      legend.key.height = unit(36, "pt"),
      legend.key.width  = unit(26, "pt"),
      legend.box.margin = margin(0, 6, 0, 6),
      legend.spacing.y  = unit(8, "pt"),
      panel.grid        = element_blank()
    )

#	Aggregate Centrality Measures
  centrality_matrix <- all_villages_centrality %>%
    mutate(measure = trimws(measure)) %>%
    filter(measure %in% c("Total Degree","Betweenness","Eigen Centrality","Closeness Out")) %>%
    mutate(
      measure_label = recode(
        measure,
        "Total Degree"     = "Degree",
        "Betweenness"      = "Betweenness",
        "Eigen Centrality" = "Eigenvector",
        "Closeness Out"    = "Closeness"
      ),
      measure_label = factor(measure_label, levels = c("Degree","Betweenness","Eigenvector","Closeness"))
    ) %>%
    group_by(group, measure_label) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      n_villages = n_distinct(village),
      .groups = "drop"
    ) %>%
    rename(value = mean_value, measure = measure_label)

#	Create Centrality Plot
  p_centrality <- ggplot(centrality_matrix,
                         aes(x = measure,
                             y = factor(group, levels = c("Periphery","Core","Most Popular")),
                             fill = value)) +
    geom_tile(color = "black", size = 1.2) +
    geom_text(aes(label = sprintf("%.2f", value)),
              size = txt_mm, color = "black", fontface = "bold", family = font_family) +
    scale_fill_gradient2(low = "#313695", mid = "#ffffbf", high = "#a50026",
                         midpoint = 0, limits = c(-1.5, 1.5), name = "Z-score") +
    scale_y_discrete(labels = c("Periphery","Core","Popular")) +
    labs(title = "C. Centrality Scores (Standardized)", x = NULL, y = NULL) +
    theme_minimal(base_family = font_family, base_size = base_pt) +
    guides(
      fill = guide_colourbar(
        direction      = "vertical",
        barheight      = unit(260, "pt"),
        barwidth       = unit(26,  "pt"),
        title.position = "top",
        ticks.colour   = "black"
      )
    ) +
    theme(
      plot.title        = element_text(size = base_pt, face = "bold", hjust = 0, colour = "black"),
      axis.text.x       = element_text(angle = 0, hjust = 0.5, size = base_pt, face = "bold", colour = "black"),
      axis.text.y       = element_text(size = base_pt, face = "bold", colour = "black"),
      legend.position   = "right",
      legend.title      = element_text(size = legend_pt, face = "bold", colour = "black"),
      legend.text       = element_text(size = legend_pt, face = "bold", colour = "black"),
      legend.key.height = unit(36, "pt"),
      legend.key.width  = unit(26, "pt"),
      legend.box.margin = margin(0, 6, 0, 6),
      legend.spacing.y  = unit(8, "pt"),
      panel.grid        = element_blank()
    )

#############################
#   Assemble Final Figure   #
#############################
  
#	Top Panel: Networks + Legend
  p_networks <- (pA | pB | pC | p_legend) +
    plot_layout(widths = c(0.9, 0.9, 0.9, 0.65)) +
    plot_annotation(
      title = "A. Village Blockmodels",
      theme = theme(
        plot.title = element_text(size = base_pt, face = "bold", hjust = 0, family = font_family)
      )
    )

#	Bottom Panel: Triad + Centrality And Shared Legend
  lower_panels <- (p_triad | p_centrality) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")

#	Combine Top And Bottom Panels
  final_plot <- p_networks / lower_panels + plot_layout(heights = c(1, 1))
  print(final_plot)

# Save Output
ggsave("role_summary.png", plot = final_plot,
       width = 43, height = 20, units = "in", dpi = 300, bg = "white")
  
  