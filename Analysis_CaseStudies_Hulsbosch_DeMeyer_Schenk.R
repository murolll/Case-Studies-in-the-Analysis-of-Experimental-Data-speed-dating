##Case studies in the Analysis of Experimental Psychology##
##Group: Judith de Meyer, Tristan Hulsbosch, Merel Schenk##
##Speed dating dataset##

##Reading in the file and filter the data####
################################################# NETWORK ANALYSIS ###########################################################


## --- Step 0: Load Required Libraries ---
library(dplyr)
library(network)
library(ergm)


## --- Step 1: Read the Data ---
# Replace 'Speed Dating Data.csv' with the correct path if necessary.
data <- read.csv("Speed Dating Data.csv", stringsAsFactors = FALSE)

# Check the structure 
str(data)

# Convert key variables to proper types:
#   iid         : Participant's unique identifier (ego)
data$iid <- as.factor(data$iid)

#   pid         : Partner's unique identifier (alter)
data$pid <- as.factor(data$pid)

#   match       : 1 if a mutual match; 0 otherwise.
data$match <- factor(data$match, levels = c(0, 1))

#   int_corr    : Correlation between interests ratings (numeric)
#   samerace    : 1 if the pair are of the same race, 0 otherwise.
data$samerace <- factor(data$samerace, levels = c(0, 1))

#   gender      : Participant's gender (0 = Female, 1 = Male).
data$gender <- factor(data$gender, levels = c(0, 1), labels = c("Female", "Male"))

#   field_cd    : Field of study (coded).
data$field_cd <- factor(data$field_cd)
# 2. Re‐level so that “12” (Undergrad/undecided) is the baseline
data$field_cd <- relevel(data$field_cd, ref = "12")

## --- Step 2: Create a List of Unique Nodes ---
nodes <- unique(c(data$iid, data$pid))
nodes <- nodes[!is.na(nodes)]  # Remove any NA values

# Number of unique individuals
num_nodes <- length(nodes)
cat("Total unique nodes:", num_nodes, "\n")

# Create a mapping from node identifier to index:
node_map <- setNames(seq_along(nodes), nodes)

## --- Step 3: Initialize the Network Object ---
net <- network.initialize(n = num_nodes, directed = FALSE)

## --- Step 4: Add Edges Based on the 'match' Variable ---
for(i in 1:nrow(data)){
  if(data$match[i] == 1) {
    ego <- as.character(data$iid[i])
    alter <- as.character(data$pid[i])
    if(!is.na(ego) && !is.na(alter)) {
      from_idx <- node_map[ego]
      to_idx   <- node_map[alter]
      if(!is.na(from_idx) && !is.na(to_idx)){
        add.edge(net, tail = from_idx, head = to_idx)
      }
    }
  }
}

## --- Step 5: Attach Node-Level Attributes ---
node_data <- data %>% 
  filter(!is.na(iid)) %>%
  group_by(iid) %>%
  summarize(gender = first(gender),
            field_cd = first(field_cd)) %>%
  ungroup()

gender_vec <- sapply(nodes, function(id) {
  val <- node_data$gender[node_data$iid == id]
  if(length(val) == 0) NA else val
})
field_vec <- sapply(nodes, function(id) {
  val <- node_data$field_cd[node_data$iid == id]
  if(length(val) == 0) NA else val
})

# Convert to character:
gender_vec_char <- as.character(gender_vec)
field_vec_char  <- as.character(field_vec)

# Replace any NA's with a placeholder value:
gender_vec_char[is.na(gender_vec_char)] <- "Missing"
field_vec_char[is.na(field_vec_char)]   <- "Missing"

# Now convert them to factors:
gender_vec_clean <- as.factor(gender_vec_char)
field_vec_clean  <- as.factor(field_vec_char)

net %v% "gender" <- as.character(gender_vec_clean)
net %v% "field_cd" <- as.character(field_vec_clean)


## --- Step 6: Create a Dyadic Covariate Matrix for 'int_corr' ---
int_corr_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
for(i in 1:nrow(data)){
  if(!is.na(data$int_corr[i])) {
    ego <- as.character(data$iid[i])
    alter <- as.character(data$pid[i])
    if(!is.na(ego) && !is.na(alter)) {
      from_idx <- node_map[ego]
      to_idx   <- node_map[alter]
      int_corr_matrix[from_idx, to_idx] <- data$int_corr[i]
      int_corr_matrix[to_idx, from_idx] <- data$int_corr[i]  # symmetric for undirected network
    }
  }
}

## --- Step 6b: Create Dyadic Covariate Matrices for Ratings ---
# For each dyad, compute the average of the participant's rating and the partner's rating.
# These ratings come from:
#  - "attr", "sinc", "intel", "fun", "amb", "shar": ratings by participant about partner.
#  - "attr_0", "sinc_0", "intel_0", "fun_0", "amb_0", "shar_0": ratings by partner about participant.
# For "like", only the participant's rating is available.

# Initialize matrices
avg_attr_matrix  <- matrix(0, nrow = num_nodes, ncol = num_nodes)
avg_sinc_matrix  <- matrix(0, nrow = num_nodes, ncol = num_nodes)
avg_intel_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
avg_fun_matrix   <- matrix(0, nrow = num_nodes, ncol = num_nodes)
avg_amb_matrix   <- matrix(0, nrow = num_nodes, ncol = num_nodes)
avg_shar_matrix  <- matrix(0, nrow = num_nodes, ncol = num_nodes)
like_matrix      <- matrix(0, nrow = num_nodes, ncol = num_nodes)

# Loop over each row to fill in the matrices
for(i in 1:nrow(data)){
  # Only consider rows with valid partner info and a match (or valid rating).
  ego <- as.character(data$iid[i])
  alter <- as.character(data$pid[i])
  if(!is.na(ego) && !is.na(alter)) {
    from_idx <- node_map[ego]
    to_idx   <- node_map[alter]
    
    # For each dyad, compute averages if both ratings are available,
    # otherwise use the available rating.
    # Attractiveness
    if(!is.na(data$attr[i]) & !is.na(data$attr_o[i])){
      avg_attr <- (data$attr[i] + data$attr_o[i]) / 2
    } else {
      avg_attr <- ifelse(!is.na(data$attr[i]), data$attr[i], data$attr_o[i])
    }
    avg_attr_matrix[from_idx, to_idx] <- avg_attr
    avg_attr_matrix[to_idx, from_idx] <- avg_attr
    
    # Sincerity
    if(!is.na(data$sinc[i]) & !is.na(data$sinc_o[i])){
      avg_sinc <- (data$sinc[i] + data$sinc_o[i]) / 2
    } else {
      avg_sinc <- ifelse(!is.na(data$sinc[i]), data$sinc[i], data$sinc_o[i])
    }
    avg_sinc_matrix[from_idx, to_idx] <- avg_sinc
    avg_sinc_matrix[to_idx, from_idx] <- avg_sinc
    
    # Intelligence
    if(!is.na(data$intel[i]) & !is.na(data$intel_o[i])){
      avg_intel <- (data$intel[i] + data$intel_o[i]) / 2
    } else {
      avg_intel <- ifelse(!is.na(data$intel[i]), data$intel[i], data$intel_o[i])
    }
    avg_intel_matrix[from_idx, to_idx] <- avg_intel
    avg_intel_matrix[to_idx, from_idx] <- avg_intel
    
    # Fun
    if(!is.na(data$fun[i]) & !is.na(data$fun_o[i])){
      avg_fun <- (data$fun[i] + data$fun_o[i]) / 2
    } else {
      avg_fun <- ifelse(!is.na(data$fun[i]), data$fun[i], data$fun_o[i])
    }
    avg_fun_matrix[from_idx, to_idx] <- avg_fun
    avg_fun_matrix[to_idx, from_idx] <- avg_fun
    
    # Ambition
    if(!is.na(data$amb[i]) & !is.na(data$amb_o[i])){
      avg_amb <- (data$amb[i] + data$amb_o[i]) / 2
    } else {
      avg_amb <- ifelse(!is.na(data$amb[i]), data$amb[i], data$amb_o[i])
    }
    avg_amb_matrix[from_idx, to_idx] <- avg_amb
    avg_amb_matrix[to_idx, from_idx] <- avg_amb
    
    # Shared interests
    if(!is.na(data$shar[i]) & !is.na(data$shar_o[i])){
      avg_shar <- (data$shar[i] + data$shar_o[i]) / 2
    } else {
      avg_shar <- ifelse(!is.na(data$shar[i]), data$shar[i], data$shar_o[i])
    }
    avg_shar_matrix[from_idx, to_idx] <- avg_shar
    avg_shar_matrix[to_idx, from_idx] <- avg_shar
    
    # Like rating 
    if(!is.na(data$like[i])){
      like_val <- data$like[i]
      like_matrix[from_idx, to_idx] <- like_val
      like_matrix[to_idx, from_idx] <- like_val
    }
  }
}

## --- Step 7: Fit the ERGM ---
# Now we add the additional dyadic covariates to the model.
#Putting al NA values to 0
avg_attr_matrix[is.na(avg_attr_matrix)]  <- 0
avg_sinc_matrix[is.na(avg_sinc_matrix)]  <- 0
avg_intel_matrix[is.na(avg_intel_matrix)] <- 0
avg_fun_matrix[is.na(avg_fun_matrix)]   <- 0
avg_amb_matrix[is.na(avg_amb_matrix)]   <- 0
avg_shar_matrix[is.na(avg_shar_matrix)]  <- 0
like_matrix[is.na(like_matrix)]          <- 0

#model 1
model <- ergm(net ~ edges +
                nodematch("field_cd") +
                edgecov(int_corr_matrix) +
                edgecov(avg_attr_matrix) +
                edgecov(avg_sinc_matrix) +
                edgecov(avg_intel_matrix) +
                edgecov(avg_fun_matrix) +
                edgecov(avg_amb_matrix) +
                edgecov(avg_shar_matrix) +
                edgecov(like_matrix),
              control = control.ergm(MCMC.burnin = 10000, MCMC.samplesize = 10000))
summary(model)



# ─────────────────────────────────────
# 1) Make A Coefficient Plot
# ─────────────────────────────────────

library(broom)
library(ggplot2)

# 1) Tidy and recode model terms
coefs <- tidy(model) %>%
  filter(term != "edges") %>%
  mutate(
    label = recode(term,
                   nodematch.field_cd        = "Same Field",
                   edgecov.int_corr_matrix   = "Interest Corr.",
                   edgecov.avg_attr_matrix   = "Attractiveness",
                   edgecov.avg_sinc_matrix   = "Sincerity",
                   edgecov.avg_intel_matrix  = "Intelligence",
                   edgecov.avg_fun_matrix    = "Fun",
                   edgecov.avg_amb_matrix    = "Ambition",
                   edgecov.avg_shar_matrix   = "Shared Interests",
                   edgecov.like_matrix       = "Liking"
    ),
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    significant = (lower > 0) | (upper < 0)       # TRUE if CI does not include zero
  )

# 2) Plot with color indicating significance
ggplot(coefs, aes(x = estimate, y = reorder(label, estimate))) +
  geom_point(aes(color = significant), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, color = significant), height = 0.2) +
  scale_color_manual(
    name   = "95% CI excludes 0",
    values = c(`TRUE` = "green3", `FALSE` = "grey30"),
    labels = c(`TRUE` = "Yes",   `FALSE` = "No")
  ) +
  labs(
    x     = "Log Odds Estimate (±95% CI)",
    y     = NULL,
    title = "ERGM Predictors of Mutual Match"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ─────────────────────────────────────
# 2) Goodness‑of‑Fit (GOF) Plots
# ─────────────────────────────────────
# Compare observed network to simulated networks on key statistics
# Goodness-of-Fit Diagnostics
gof_obj <- gof(model ~ degree + espartners + distance)
plot(gof_obj)


###########################################################################################
#  network visualisations
###########################################################################################


###########################################################################################
#observed (matches) network
###########################################################################################

library(igraph)
library(ggraph)
library(ggplot2) 

# 1) Extract all observed mutual matches from your data frame
#    (make sure data$iid and data$pid are character)
obs_edges <- data %>%
  filter(match == 1) %>%
  transmute(from = iid, to = pid)

# 2) Build an igraph from those edges
ig_obs <- graph_from_data_frame(obs_edges, directed = FALSE, vertices = nodes)
#    Note: vertices=nodes ensures isolated participants appear too

# 3) Attach node attributes (gender, field, etc.)
V(ig_obs)$gender <- (data %>% distinct(iid, gender))$gender[
  match(V(ig_obs)$name, data %>% distinct(iid) %>% pull(iid))
]
#    If you also want field_cd, add similarly.

# 4) (Optional) Prune isolates to focus on the actual matching subgraph
ig_obs_sub <- delete_vertices(ig_obs, which(degree(ig_obs) == 0))

# 5) Plot the observed‐match network
ggraph(ig_obs_sub, layout = "fr") +
  geom_edge_link(color = "grey70", width = 0.8) +
  geom_node_point(aes(color = gender, size = degree(ig_obs_sub)),
                  show.legend = TRUE) +
  geom_node_text(aes(label = ifelse(degree(ig_obs_sub) > 1, name, "")),
                 repel = TRUE, size = 3) +
  scale_size_continuous(name = "Matches (degree)", range = c(3, 8)) +
  labs(
    title = "Observed Mutual Matches Network",
    subtitle = sprintf("N edges = %d", ecount(ig_obs_sub)),
    color = "Gender"
  ) +
  theme_void() 

###########################################################################################
#### MARGINAL EFFECTS SCATTERPLOT
###########################################################################################

# 1a) Build a dyad‐level data frame of predictors + predicted p
dyads <- expand.grid(i = 1:num_nodes, j = 1:num_nodes) %>%
  filter(i < j)              # undirected: one row per pair
pred_df <- dyads %>%
  transmute(
    avg_attr  = avg_attr_matrix[cbind(i, j)],
    avg_fun   = avg_fun_matrix [cbind(i, j)],
    avg_shar  = avg_shar_matrix[cbind(i, j)],
    avg_amb  = avg_amb_matrix[cbind(i, j)],
    avg_intel  = avg_intel_matrix[cbind(i, j)],
    avg_sinc  = avg_sinc_matrix[cbind(i, j)],
    like      = like_matrix [cbind(i, j)]
  )

# 1b) Add linear predictor & probability from the model
β <- coef(model)
pred_df <- pred_df %>%
  mutate(
    η = β["edges"] +
      β["edgecov.avg_attr_matrix"] * avg_attr +
      β["edgecov.avg_fun_matrix"]  * avg_fun  +
      β["edgecov.avg_shar_matrix"] * avg_shar +
      β["edgecov.avg_sinc_matrix"] * avg_sinc +
      β["edgecov.avg_amb_matrix"] * avg_amb +
      β["edgecov.avg_intel_matrix"] * avg_intel +
      β["edgecov.like_matrix"]     * like,
    p = plogis(η)
  )

# 1c) Example: p vs. avg_x
ggplot(pred_df, aes(x = like, y = p)) +
  geom_jitter(height = 0.02, alpha = 0.1) +
  geom_smooth(method = "loess", color = "steelblue") +
  labs(
    x = "Average Like Ratings",
    y = "Predicted Match Probability",
    title = "Marginal Effect of General Liking on Match Odds"
  ) +
  theme_minimal()








###########################################################################################

######## Predicted matches network (predictions based on model!)
###########################################################################################


# 0) we already have these
#    - model           :  fitted ERGM
#    - num_nodes       : number of nodes in the original network
#    - net             : the original network object with net %v% "gender"
#    - int_corr_matrix, avg_attr_matrix, avg_sinc_matrix,
#      avg_intel_matrix, avg_fun_matrix, avg_amb_matrix,
#      avg_shar_matrix, like_matrix : your dyadic matrices

# 1) Extract coefficients and build linear predictor
cf <- coef(model)
LP_full <- matrix(cf["edges"], nrow = num_nodes, ncol = num_nodes)
LP_full <- LP_full +
  cf["edgecov.int_corr_matrix"] * int_corr_matrix +
  cf["edgecov.avg_attr_matrix"] * avg_attr_matrix +
  cf["edgecov.avg_sinc_matrix"] * avg_sinc_matrix +
  cf["edgecov.avg_intel_matrix"] * avg_intel_matrix +
  cf["edgecov.avg_fun_matrix"]  * avg_fun_matrix +
  cf["edgecov.avg_amb_matrix"]  * avg_amb_matrix +
  cf["edgecov.avg_shar_matrix"] * avg_shar_matrix +
  cf["edgecov.like_matrix"]     * like_matrix

# 2) Convert to probabilities
plot_prob_mat <- 1 / (1 + exp(-LP_full))

# 3) Threshold into backbone adjacency
plot_threshold <- 0.60
plot_adj_mat <- (plot_prob_mat > plot_threshold) | t(plot_prob_mat > plot_threshold)
diag(plot_adj_mat) <- FALSE

# 4) Build a fresh igraph from that adjacency
plot_ig <- graph_from_adjacency_matrix(plot_adj_mat, mode = "undirected")
plot_ig <- delete_vertices(plot_ig, which(degree(plot_ig) == 0))  # optional prune

# 5) Prepare node attributes
#    Convert the original 'nodes' factor into a character vector
plot_nodes <- as.character(nodes)

#    Assign vertex names and gender
V(plot_ig)$plot_name   <- plot_nodes[as.integer(V(plot_ig))]
V(plot_ig)$plot_gender <- (net %v% "gender")[ match(V(plot_ig)$plot_name, nodes) ]

# 6) Attach edge probabilities
el <- as_edgelist(plot_ig, names = FALSE)
plot_edge_prob <- vapply(
  seq_len(nrow(el)),
  function(k) plot_prob_mat[ el[k,1], el[k,2] ],
  numeric(1)
)
E(plot_ig)$plot_prob <- plot_edge_prob

# 7) Plot with ggraph (no variables overwritten)
ggraph(plot_ig, layout = "fr") +
  geom_edge_link(aes(width = plot_prob), alpha = 0.6, show.legend = TRUE) +
  geom_node_point(aes(color = plot_gender, size = degree(plot_ig)),
                  show.legend = TRUE) +
  geom_node_text(aes(label = ifelse(degree(plot_ig) > 2, plot_name, "")),
                 repel = TRUE, size = 3) +
  scale_edge_width_continuous(name = "P(match)", range = c(0.2, 2)) +
  scale_size_continuous(name = "Degree", range = c(3, 8)) +
  labs(
    title = sprintf("Predicted Match Network (p > %.2f)", plot_threshold),
    color = "Gender"
  ) +
  theme_void()















##############################
# IGNORE BELOW: does not work #
##############################
#model 2, directed ergm about possible decision per participant about partner#

# # IDs as characters for mapping
# data$iid <- as.character(data$iid)
# data$pid <- as.character(data$pid)
# 
# # Node‐level covariates to factor
# data$dec      <- as.numeric(data$dec)    # 1=yes,0=no
# 
# # Convert samerace to 0/1
# data$samerace <- ifelse(data$samerace == 1, 1, 0)
# 
# # -----------------------------------------------------
# # 2) Build directed network from 'dec'
# # -----------------------------------------------------
# # 2a) Node list & mapping
# nodes     <- unique(c(data$iid, data$pid))
# nodes <- nodes[!is.na(nodes)]  # remove NA if needed
# num_nodes <- length(nodes)
# node_map  <- setNames(seq_along(nodes), nodes)
# 
# # 2b) Initialize directed network
# net <- network.initialize(n = num_nodes, directed = TRUE)
# net %v% "uid" <- nodes
# 
# # 2c) Add an edge i->j whenever dec == 1
# for(i in seq_len(nrow(data))) {
#   if(!is.na(data$dec[i]) && data$dec[i] == 1) {
#     tail <- node_map[data$iid[i]]
#     head <- node_map[data$pid[i]]
#     if(!is.na(tail) && !is.na(head))
#       add.edge(net, tail = tail, head = head)
#   }
# }
# 
# # -----------------------------------------------------
# # 3) Attach node‐level attributes
# # -----------------------------------------------------
# node_data <- data %>%
#   filter(!is.na(iid)) %>%
#   group_by(iid) %>%
#   summarize(
#     gender   = first(gender),
#     field_cd = first(field_cd),
#     race     = first(race)
#   ) %>%
#   ungroup()
# 
# net %v% "gender"   <- as.character(node_data$gender   [match(nodes, node_data$iid)])
# net %v% "field_cd" <- as.character(node_data$field_cd [match(nodes, node_data$iid)])
# net %v% "race"     <- as.character(node_data$race[match(nodes, node_data$iid)])
# 
# # — Step 3b: Impute missing field_cd as “12” (Undergrad/Undecided) —
# fc <- net %v% "field_cd"
# fc <- as.character(fc)
# fc[is.na(fc)] <- "12" 
# # Re‐factor using the same levels present in your original data
# net %v% "field_cd" <- as.character(factor(fc, levels = levels(data$field_cd)))
# 
# # -----------------------------------------------------
# # 4) Impute missing gender by strictly‐heterosexual design
# # -----------------------------------------------------
# g   <- net %v% "gender"
# adj <- as.matrix(net, matrix.type = "adjacency")
# 
# for (v in which(is.na(g))) {
#   # Find incoming and outgoing neighbors
#   in_neis  <- which(adj[, v] == 1)
#   out_neis <- which(adj[v, ] == 1)
#   all_neis <- unique(c(in_neis, out_neis))
# 
#   # Filter to neighbors with non‐NA gender
#   valid_neis <- all_neis[!is.na(g[all_neis])]
# 
#   if (length(valid_neis) > 0) {
#     # Flip based on the first valid neighbor
#     nbr_gender    <- g[valid_neis[1]]
#     g[v] <- if (nbr_gender == "Female") "Male" else "Female"
#   } else {
#     # If truly isolated or all neighbors unknown, assign a default
#     g[v] <- "Female"
#   }
# }
# 
# # Re‐attach back to the network (ensure factor levels)
# net %v% "gender" <- as.character(factor(g, levels = c("Female", "Male")))
# 
# # -----------------------------------------------------
# # 5) Build dyadic (edgecov) matrices
# # -----------------------------------------------------
# make_mat <- function() {
#   matrix(NA, nrow = num_nodes, ncol = num_nodes, dimnames = list(nodes, nodes))
# }
# 
# # Initialize matrices
# int_corr_mat <- make_mat()
# order_mat    <- make_mat()
# samerace_mat <- make_mat()
# 
# attr_mat     <- make_mat()
# sinc_mat     <- make_mat()
# intel_mat    <- make_mat()
# fun_mat      <- make_mat()
# amb_mat      <- make_mat()
# shar_mat     <- make_mat()
# like_mat     <- make_mat()
# 
# 
# # Fill them
# for (i in seq_len(nrow(data))) {
#   ego   <- data$iid[i]
#   alter <- data$pid[i]
#   
#   # Skip if missing
#   if (is.na(ego) || is.na(alter)) next
#   if (!(ego %in% names(node_map)) || !(alter %in% names(node_map))) next
#   
#   ri <- node_map[[ego]]
#   rj <- node_map[[alter]]
#   
#   int_corr_mat[ri, rj] <- data$int_corr[i]
#   order_mat   [ri, rj] <- data$order[i]
#   samerace_mat[ri, rj] <- data$samerace[i]
#   
#   attr_mat [ri, rj] <- data$attr[i]
#   sinc_mat [ri, rj] <- data$sinc[i]
#   intel_mat[ri, rj] <- data$intel[i]
#   fun_mat  [ri, rj] <- data$fun[i]
#   amb_mat  [ri, rj] <- data$amb[i]
#   shar_mat [ri, rj] <- data$shar[i]
#   like_mat [ri, rj] <- data$like[i]
# }
# 
# 
# # Replace NAs with 0
# # Replace all NAs with 0 (again, just in case)
# int_corr_mat[is.na(int_corr_mat)] <- 0
# order_mat[is.na(order_mat)]       <- 0
# samerace_mat[is.na(samerace_mat)] <- 0
# 
# attr_mat[is.na(attr_mat)]   <- 0
# sinc_mat[is.na(sinc_mat)]   <- 0
# intel_mat[is.na(intel_mat)] <- 0
# fun_mat[is.na(fun_mat)]     <- 0
# amb_mat[is.na(amb_mat)]     <- 0
# shar_mat[is.na(shar_mat)]   <- 0
# like_mat[is.na(like_mat)]   <- 0
# 
# 
# # -----------------------------------------------------
# # 6) Fit the Directed ERGM
# # -----------------------------------------------------
# model_directed <- ergm(
#   net ~ edges
#   # node‐level
#   + nodefactor("gender")
#   + nodematch ("field_cd")
#   # + nodefactor("race")
#   # + nodematch ("race")
#   # dyadic‐level
#   + edgecov(int_corr_mat)
#   + edgecov(order_mat)
#   + edgecov(samerace_mat)
#   + edgecov(attr_mat)
#   + edgecov(sinc_mat)
#   + edgecov(intel_mat)
#   + edgecov(fun_mat)
#   + edgecov(amb_mat)
#   + edgecov(shar_mat)
#   + edgecov(like_mat),
#   control = control.ergm(
#     MCMC.burnin     = 10000,
#     MCMC.samplesize = 10000
#   )
# )
# 
# summary(model_directed)




# -----------------------------------------------------
# 6) (Optional) Goodness‐of‐Fit
# -----------------------------------------------------
# gof_out <- gof(model_directed ~ degree + distance + espartners)
# plot(gof_out)




# # -----------------------------------------------------
# # 0) Preliminaries
# # -----------------------------------------------------
# library(dplyr)
# library(network)
# library(ergm)
# 
# # -----------------------------------------------------
# # 1) Data Prep
# # -----------------------------------------------------
# data <- read.csv("Speed Dating Data.csv", stringsAsFactors = FALSE)
# 
# data$iid <- as.character(data$iid)
# data$pid <- as.character(data$pid)
# 
# # Outcome
# data$dec      <- as.numeric(data$dec)       # 1 = yes, 0 = no
# data$samerace <- ifelse(data$samerace == 1, 1, 0)
# 
# # -----------------------------------------------------
# # 2) Build Directed Network
# # -----------------------------------------------------
# nodes     <- unique(c(data$iid, data$pid))
# nodes     <- nodes[!is.na(nodes)]
# num_nodes <- length(nodes)
# node_map  <- setNames(seq_along(nodes), nodes)
# 
# net <- network.initialize(n = num_nodes, directed = TRUE)
# net %v% "uid" <- nodes
# 
# # 2c) Add an edge i->j whenever dec == 1, but only if both IDs map
# for (i in seq_len(nrow(data))) {
#   if (!is.na(data$dec[i]) && data$dec[i] == 1) {
#     ego   <- data$iid[i]
#     alter <- data$pid[i]
#     
#     # Skip if either ID is missing or not in node_map
#     if (is.na(ego)      || !(ego   %in% names(node_map))) next
#     if (is.na(alter)    || !(alter %in% names(node_map))) next
#     
#     tail <- node_map[[ego]]
#     head <- node_map[[alter]]
#     
#     # Finally, only add if both indices are valid
#     if (!is.na(tail) && !is.na(head)) {
#       add.edge(net, tail = tail, head = head)
#     }
#   }
# }
# 
# # -----------------------------------------------------
# # 3) Attach and Impute Node‐Level Attributes
# # -----------------------------------------------------
# node_data <- data %>%
#   filter(!is.na(iid)) %>%
#   group_by(iid) %>%
#   summarize(
#     gender   = first(gender),
#     field_cd = first(field_cd),
#     race     = first(race)
#   ) %>%
#   ungroup()
# 
# # Assign raw (character) then impute
# net %v% "gender"   <- node_data$gender  [match(nodes, node_data$iid)]
# net %v% "field_cd" <- node_data$field_cd[match(nodes, node_data$iid)]
# net %v% "race"     <- node_data$race    [match(nodes, node_data$iid)]
# 
# # Impute missing gender by heterosexual pairing
# g   <- net %v% "gender"
# adj <- as.matrix(net, matrix.type = "adjacency")
# for (v in which(is.na(g))) {
#   neis      <- unique(c(which(adj[,v]==1), which(adj[v,]==1)))
#   valid_nei <- neis[!is.na(g[neis])]
#   if (length(valid_nei)>0) {
#     g[v] <- if (g[valid_nei[1]]=="Female") "Male" else "Female"
#   } else {
#     g[v] <- "Female"
#   }
# }
# net %v% "gender" <- as.character(factor(g, levels = c("Female","Male")))
# 
# # Impute missing field_cd → “12”
# fc <- net %v% "field_cd"; fc[is.na(fc)] <- "12"
# net %v% "field_cd" <- as.character(factor(fc, levels = unique(data$field_cd)))
# 
# # -----------------------------------------------------
# # 4) Prepare Gender‐Specific Edgecovariates
# # -----------------------------------------------------
# make_mat <- function() {
#   matrix(NA, nrow=num_nodes, ncol=num_nodes,
#          dimnames=list(nodes,nodes))
# }
# 
# # Initialize two matrices per trait
# traits    <- c("attr","sinc","intel","fun","amb","shar","like")
# male_mats <- setNames(lapply(traits, function(x) make_mat()), paste0("male_",traits,"_mat"))
# fem_mats  <- setNames(lapply(traits, function(x) make_mat()), paste0("female_",traits,"_mat"))
# 
# # Also retain other dyadic covs
# int_corr_mat <- make_mat()
# order_mat    <- make_mat()
# samerace_mat <- make_mat()
# 
# # Fill them
# for (i in seq_len(nrow(data))) {
#   ego   <- data$iid[i]
#   alter <- data$pid[i]
#   if (is.na(ego) || is.na(alter)) next
#   if (!(ego %in% nodes) || !(alter %in% nodes)) next
#   
#   ri    <- node_map[[ego]]
#   rj    <- node_map[[alter]]
#   ego_g <- (net %v% "gender")[ri]
#   
#   # skip if gender still missing
#   if (is.na(ego_g)) next
#   
#   # fill the generic dyadic covariates
#   int_corr_mat[ri, rj] <- data$int_corr[i]
#   order_mat   [ri, rj] <- data$order[i]
#   samerace_mat[ri, rj] <- data$samerace[i]
#   
#   # fill the gender‐specific trait matrices
#   for (t in traits) {
#     val <- data[[t]][i]
#     if (is.na(val)) next
#     
#     if (ego_g == "Male") {
#       male_mats[[paste0("male_", t, "_mat")]] [ri, rj] <- val
#     } else {
#       fem_mats [[paste0("female_", t, "_mat")]][ri, rj] <- val
#     }
#   }
# }
# 
# 
# # Zero‐fill all matrices
# fill_zero <- function(mat) { mat[is.na(mat)] <- 0; mat }
# int_corr_mat <- fill_zero(int_corr_mat)
# order_mat    <- fill_zero(order_mat)
# samerace_mat <- fill_zero(samerace_mat)
# for (nm in names(male_mats)) { male_mats[[nm]] <- fill_zero(male_mats[[nm]]) }
# for (nm in names(fem_mats))  { fem_mats[[nm]]  <- fill_zero(fem_mats[[nm]]) }
# 
# # Expose variables for ERGM
# list2env(male_mats, envir=.GlobalEnv)
# list2env(fem_mats,  envir=.GlobalEnv)
# 
# 
# # -----------------------------------------------------
# # Right before your ergm(...) call
# # -----------------------------------------------------
# 
# # 1) Pull out the current gender attribute as character
# current_gender <- as.character(net %v% "gender")
# 
# # 2) Impute any remaining NAs as "Female"
# current_gender[is.na(current_gender)] <- "Female"
# 
# # 3) Turn that vector into a proper factor of the correct length
# fixed_gender <- factor(current_gender,
#                        levels = c("Female","Male"))
# 
# # 4) Assign back to the network with the network:: setter
# network::set.vertex.attribute(net,
#                               attrname = "gender",
#                               value    = fixed_gender)
# 
# # 5) Quick sanity check—no NAs, two levels only:
# table(net %v% "gender", useNA="ifany")
# # Should print something like:
# # Female   Male 
# #    300    250  
# 
# # Now you can safely call:
# model_gender_split <- ergm(
#   net ~ edges
#   + nodefactor("gender")
#   + nodematch("field_cd")
#   + edgecov(int_corr_mat)
#   + edgecov(order_mat)
#   + edgecov(samerace_mat)
#   + edgecov(male_attr_mat)   + edgecov(female_attr_mat)
#   + edgecov(male_sinc_mat)   + edgecov(female_sinc_mat)
#   + edgecov(male_intel_mat)  + edgecov(female_intel_mat)
#   + edgecov(male_fun_mat)    + edgecov(female_fun_mat)
#   + edgecov(male_amb_mat)    + edgecov(female_amb_mat)
#   + edgecov(male_shar_mat)   + edgecov(female_shar_mat)
#   + edgecov(male_like_mat)   + edgecov(female_like_mat),
#   control = control.ergm(
#     MCMC.burnin     = 10000,
#     MCMC.samplesize = 10000
#   )
# )
# 
# summary(model_gender_split)
# 
# 
# # 6) Inspect Results
# summary(model_gender_split)


