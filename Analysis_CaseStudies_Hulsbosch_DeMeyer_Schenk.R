##Case studies in the Analysis of Experimental Psychology##
##Group: Judith de Meyer, Tristan Hulsbosch, Merel Schenk##
##Speed dating dataset##

##Reading in the file and filter the data####
################################################# NETWORK ANALYSIS ###########################################################
##Case studies in the Analysis of Experimental Psychology##
##Group: Judith de Meyer, Tristan Hulsbosch, Merel Schenk##
##Speed dating dataset##

## --- Step 0: Load Required Libraries ---
library(dplyr)
library(network)
library(ergm)
library(igraph)
library(intergraph)

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
  # Only consider rows with valid partner info and a match (or valid rating), if you wish to restrict to dyads with dates.
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
    
    # Like rating (only participant's rating available)
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
# In many ERGM applications, researchers fill in missing dyadic covariate values with 0 (or with a constant) if the absence is considered “no similarity” or “no signal.”
# Alternatively, if an NA indicates that a dyad never occurred (and thus should not contribute), you may set that to 0.

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


