##Case studies in the Analysis of Experimental Psychology##
##Group: Judith de Meyer, Tristan Hulsbosch, Merel Schenk##
##Speed dating dataset##

############################# ERGM NETWORK ANALYSIS #################################################


## --- Step 0: Load Required Libraries ---
library(dplyr)
library(network)
library(ergm)


## --- Step 1: Read the Data ---
# Replace 'Speed Dating Data.csv' with the correct path if necessary.
data <- read.csv("Speed Dating Data.csv", stringsAsFactors = FALSE)

# Check the structure of your data
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

# Now convert them to factors if needed:
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
  # Only consider rows with valid partner info and a match (or valid rating)
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


#### Final ERGM model
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
# 1) Make a coefficient plot of the ERGM model estimates
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







###########################################################################################
#  network visualisations
###########################################################################################


############################################
#observed (matches) network
############################################

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

# 4) Prune isolates to focus on the actual matching subgraph
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
#### MARGINAL EFFECTS SCATTERPLOTs
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

# 1c) Example: p vs. avg_XXX -- CHANGE VALUES TO OBTAIN EACH SEPARATE MARGINAL EFFECTS PLOT!
ggplot(pred_df, aes(x = avg_attr, y = p)) +
  geom_jitter(height = 0.02, alpha = 0.1) +
  geom_smooth(method = "loess", color = "steelblue") +
  labs(
    x = "Average Attraction Ratings",
    y = "Predicted Match Probability",
    title = "Marginal Effect of Avg Attraction on Match Odds"
  ) +
  theme_minimal()
