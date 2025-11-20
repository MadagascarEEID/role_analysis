# Typology of 36 Triad Positions
# Created by Tyler Barrett on October 1, 2025
# This code generates the triad typology figure. No data are required.

#################
#   PREAMBLE    #
#################

#	Load packages
  library(tidyverse)
  library(ggh4x)
  library(patchwork)

##################
#   FUNCTIONS    #
##################

#	Extract Edge Indices from Adjacency Matrix
  edge_index <- function(adj) {
    
    #	Find edge positions
      idx <- which(adj == 1, arr.ind = TRUE)
    
    #	Early return if no edges
      if (length(idx) == 0) {
        return(tibble(from = integer(), to = integer()))
      }
    
    #	Convert to tibble
      as_tibble(idx) %>% rename(from = row, to = col)
}

#	Derive Role Group from Focal Position
  role_from_focal <- function(adj, focal) {
    
    #	Identify other nodes
      others <- setdiff(1:3, focal)
    
    #	Classify relationships
      rel <- vapply(others, function(j) {
        a <- adj[focal, j]
        b <- adj[j, focal]
        if (a == 1 && b == 1) "mutual"
        else if (a == 1)      "out"
        else if (b == 1)      "in"
        else                  "none"
      }, character(1))
    
    #	Count tie types
      m <- sum(rel == "mutual")
      o <- sum(rel == "out")
      i <- sum(rel == "in")
    
    #	Assign role based on counts
      if (m == 0 && o == 0 && i == 0) return("Isolate")
      if (m == 1 && o == 0 && i == 0) return("Reciprocal")
      if (m == 0 && o == 1 && i == 0) return("Unreciprocated out")
      if (m == 0 && o == 0 && i == 1) return("Unreciprocated in")
      if (m == 2)                     return("Two reciprocal")
      if (o == 2)                     return("Unreciprocated out (hub)")
      if (i == 2)                     return("Unreciprocated in (hub)")
      if (m == 1 && o == 1)           return("Reciprocal + outward")
      if (m == 1 && i == 1)           return("Reciprocal + inward")
      if (o == 1 && i == 1)           return("In & out (bridge)")
      "Other"
}

#	Shorten Edges for Arrow Visibility
  shorten_edges <- function(df, r = 0.22) {
    
    #	Calculate edge vectors
      dx  <- df$x_to - df$x_from
      dy  <- df$y_to - df$y_from
      len <- sqrt(dx^2 + dy^2)
      len[len == 0] <- 1e-9
    
    #	Shorten from both ends
      df$x_from <- df$x_from + r * dx / len
      df$y_from <- df$y_from + r * dy / len
      df$x_to   <- df$x_to   - r * dx / len
      df$y_to   <- df$y_to   - r * dy / len
    
    return(df)
}

#	Plot Triad Group
  plot_group <- function(group_name, title) {
    
    #	Filter triads for this group
      triads <- nodes_df %>%
        filter(role_group == group_name) %>%
        pull(triad) %>%
        unique()
    
    #	Build plot
      ggplot() +
        geom_rect(
          data = panel_bg %>% filter(triad %in% triads),
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          color = "grey70", fill = NA, linewidth = 0.4
        ) +
        geom_segment(
          data = edges_df_short %>% filter(triad %in% triads),
          aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
          arrow = arrow(length = unit(1.6, "mm"), type = "closed"),
          linewidth = 0.5, lineend = "round", color = "black"
        ) +
        geom_point(
          data = nodes_df %>% filter(triad %in% triads),
          aes(x = x, y = y, color = focal),
          size = 2.2, stroke = 0.3
        ) +
        scale_color_manual(values = c("black", "red"), guide = "none") +
        coord_fixed(xlim = c(-1.08, 1.08), ylim = c(-1.08, 1.08), expand = FALSE) +
        facet_wrap(~ triad, ncol = length(triads)) +
        labs(title = title) +
        inner_theme
}

#	Wrap Plot for Patchwork
  wrap_plot <- function(p) {
    wrap_elements(full = p) + plot_layout(widths = 1, heights = 1)
}

##############################
#   FIXED NODE COORDINATES   #
##############################

node_coords <- tibble(
  node = 1:3,
  x    = c(0, -0.6, 0.6),
  y    = c(0.7, -0.35, -0.35)
)

###################################
#   ADJACENCY MATRIX DEFINITIONS  #
###################################

#	Base triad structures
  A003  <- matrix(0, 3, 3)
  A012  <- matrix(c(0,1,0, 0,0,0, 0,0,0), 3,3, byrow = TRUE)
  A102  <- matrix(c(0,1,0, 1,0,0, 0,0,0), 3,3, byrow = TRUE)
  A021D <- matrix(c(0,1,1, 0,0,0, 0,0,0), 3,3, byrow = TRUE)
  A021U <- matrix(c(0,0,0, 1,0,0, 1,0,0), 3,3, byrow = TRUE)
  A021C <- matrix(c(0,1,0, 0,0,1, 0,0,0), 3,3, byrow = TRUE)
  A111D <- matrix(c(0,1,1, 1,0,0, 0,0,0), 3,3, byrow = TRUE)
  A111U <- matrix(c(0,1,0, 1,0,0, 1,0,0), 3,3, byrow = TRUE)
  A030T <- matrix(c(0,1,1, 0,0,1, 0,0,0), 3,3, byrow = TRUE)
  A030C <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3,3, byrow = TRUE)
  A201  <- matrix(c(0,1,1, 1,0,0, 1,0,0), 3,3, byrow = TRUE)
  A120D <- matrix(c(0,1,1, 1,0,1, 0,0,0), 3,3, byrow = TRUE)
  A120U <- matrix(c(0,1,0, 1,0,0, 1,1,0), 3,3, byrow = TRUE)
  A120C <- matrix(c(0,1,0, 1,0,1, 1,0,0), 3,3, byrow = TRUE)
  A210  <- matrix(c(0,1,1, 1,0,1, 1,0,0), 3,3, byrow = TRUE)
  A300  <- matrix(c(0,1,1, 1,0,1, 1,1,0), 3,3, byrow = TRUE)

#	Collect into named list
  base_mats <- list(
    "003" = A003, "012" = A012, "102" = A102,
    "021D" = A021D, "021U" = A021U, "021C" = A021C,
    "111D" = A111D, "111U" = A111U,
    "030T" = A030T, "030C" = A030C,
    "201"  = A201,  "120D" = A120D, "120U" = A120U, "120C" = A120C,
    "210"  = A210,  "300" = A300
  )

###########################
#   TRIAD SPECIFICATIONS  #
###########################

#	Original triad-base-focal mapping
  triad_specs_original <- tribble(
    ~triad,   ~base,  ~focal,
    "003",    "003",  1,
    "012_S",  "012",  1,
    "012_E",  "012",  2,
    "012_I",  "012",  3,
    "102_D",  "102",  1,
    "102_I",  "102",  3,
    "021D_S", "021D", 1,
    "021D_E", "021D", 2,
    "021U_S", "021U", 2,
    "021U_E", "021U", 1,
    "021C_S", "021C", 1,
    "021C_B", "021C", 2,
    "021C_E", "021C", 3,
    "111D_S", "111D", 1,
    "111D_B", "111D", 2,
    "111D_E", "111D", 3,
    "111U_S", "111U", 1,
    "111U_B", "111U", 2,
    "111U_E", "111U", 3,
    "030T_S", "030T", 1,
    "030T_B", "030T", 2,
    "030T_E", "030T", 3,
    "030C",   "030C", 1,
    "201_S",  "201",  1,
    "201_B",  "201",  2,
    "120D_S", "120D", 1,
    "120D_E", "120D", 3,
    "120U_S", "120U", 1,
    "120U_E", "120U", 3,
    "120C_S", "120C", 2,
    "120C_B", "120C", 1,
    "120C_E", "120C", 3,
    "210_S",  "210",  1,
    "210_B",  "210",  2,
    "210_E",  "210",  3,
    "300",    "300",  1
  )

#	Label corrections mapping
  corrections <- tribble(
    ~old,      ~new,
    "111U_E",  "111D_S",
    "111D_E",  "111U_E",
    "120U_E",  "120D_S",
    "120D_E",  "120U_E",
    "120C_E",  "120C_B",
    "111D_B",  "111U_S",
    "111U_B",  "111D_E",
    "201_B",   "201_S",
    "201_S",   "201_B",
    "210_S",   "210_B",
    "111D_S",  "111U_B",
    "120D_S",  "120U_S",
    "210_B",   "210_S",
    "111U_S",  "111D_B",
    "120C_B",  "120C_E",
    "120U_S",  "120D_E"
  )

#	Apply corrections
  triad_specs <- triad_specs_original %>%
    left_join(corrections, by = c("triad" = "old")) %>%
    mutate(triad = coalesce(new, triad)) %>%
    select(triad, base, focal)

#	Validation
  if (n_distinct(triad_specs$triad) != nrow(triad_specs)) {
    stop("ERROR: Duplicate triads found after corrections!")
  }

########################################
#   EDGE AND NODE DATA CONSTRUCTION    #
########################################

#	Build edge data frame with coordinates
  edges_df <- triad_specs %>%
    rowwise() %>%
    mutate(
      edges = list(
        edge_index(base_mats[[base]]) %>%
          left_join(node_coords, by = c("from" = "node")) %>%
          rename(x_from = x, y_from = y) %>%
          left_join(node_coords, by = c("to" = "node")) %>%
          rename(x_to = x, y_to = y)
      )
    ) %>%
    ungroup() %>%
    select(triad, edges) %>%
    unnest(edges, keep_empty = TRUE)

#	Build node data frame with focal indicator
  nodes_df <- triad_specs %>%
    rowwise() %>%
    mutate(nodes = list(node_coords %>% mutate(focal = (node == focal)))) %>%
    ungroup() %>%
    select(triad, nodes) %>%
    unnest(nodes)

#	Panel background rectangles
  panel_bg <- triad_specs %>%
    transmute(triad, xmin = -1.05, xmax = 1.05, ymin = -1.05, ymax = 1.05)

############################################
#   TRIAD POSITION GROUP CLASSIFICATION    #
############################################

#	Compute groups for all triads
  role_map <- triad_specs %>%
    rowwise() %>%
    mutate(role_group = role_from_focal(base_mats[[base]], focal)) %>%
    ungroup() %>%
    select(triad, role_group)

#	Attach groups to data frames
  nodes_df <- nodes_df %>% left_join(role_map, by = "triad")
  edges_df <- edges_df %>% left_join(role_map, by = "triad")
  panel_bg <- panel_bg %>% left_join(role_map, by = "triad")

#########################################
#   EDGE SHORTENING FOR ARROW DISPLAY   #
#########################################

#	Apply shortening
  edges_df_short <- shorten_edges(edges_df, r = 0.22)

########################
#   PLOTTING THEMES    #
########################

#	Internal plot theme for triad panels
  inner_theme <- theme_void(base_size = 10.5) +
    theme(
      plot.title       = element_text(size = 10, face = "bold", hjust = 0.5,
                                      margin = margin(t = 0, b = 1)),
      panel.spacing    = unit(1, "mm"),
      strip.background = element_rect(fill = "grey95", color = "grey80",
                                      linewidth = 0.3),
      strip.text       = element_text(size = 8.5),
      plot.margin      = margin(1, 1, 1, 1)
    )

################################
#   GENERATE CATEGORY PANELS   #
################################

#	Create wrapped plots for each role group
  pA <- wrap_plot(plot_group("Isolate",                   "Isolate"))
  pB <- wrap_plot(plot_group("Unreciprocated out",        "Unreciprocated Out"))
  pC <- wrap_plot(plot_group("Unreciprocated in",         "Unreciprocated In"))
  pD <- wrap_plot(plot_group("Unreciprocated out (hub)",  "Unreciprocated Out (Hub)"))
  pE <- wrap_plot(plot_group("Unreciprocated in (hub)",   "Unreciprocated In (Hub)"))
  pF <- wrap_plot(plot_group("In & out (bridge)",         "Bridge"))
  pG <- wrap_plot(plot_group("Reciprocal",                "One Reciprocal"))
  pH <- wrap_plot(plot_group("Two reciprocal",            "Two Reciprocal"))
  pI <- wrap_plot(plot_group("Reciprocal + outward",      "Reciprocal and Unreciprocated Out"))
  pJ <- wrap_plot(plot_group("Reciprocal + inward",       "Reciprocal and Unreciprocated In"))

########################
#   ASSEMBLE LAYOUT    #
########################

#	Build rows
  row1 <- pA
  row2 <- pB + pC
  row3 <- pD + pE
  row4 <- pF
  row5 <- pG + pH
  row6 <- pI + pJ

#	Stack into final layout
  triad_plot <- (row1 / row2 / row3 / row4 / row5 / row6) +
    plot_layout(ncol = 1) +
    plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

#	Display
  triad_plot

###################
#   SAVE OUTPUT   #
###################

# ggsave("triad_plot.png", triad_plot, width = 10, height = 8, dpi = 300)
  
  